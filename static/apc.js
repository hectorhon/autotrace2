var duration = end - start;

uptimes.forEach(function(uptime) {
    uptime.start = new Date(uptime.start);
    uptime.end = new Date(uptime.end);
    uptime.duration = uptime.end - uptime.start;

    uptime.eStart = new Date(Math.max(uptime.start, start));
    uptime.eEnd = new Date(Math.min(uptime.end, end));
    uptime.eDuration = uptime.eEnd - uptime.eStart;

    $("#uptimes").append(
        $("<div></div>").html("&nbsp").addClass("uptime-bar").css({
            "width": uptime.eDuration / duration * 100 + "%",
            "margin-left": (uptime.eStart - start) / duration * 100 + "%"
        })
    );
});

issues.forEach(function(issue) {
    issue.start = new Date(issue.start);
    issue.end = new Date(issue.end);
    issue.duration = issue.end - issue.start;

    issue.eStart = new Date(Math.max(issue.start, start));
    issue.eEnd = new Date(Math.min(issue.end, end));
    issue.eDuration = issue.eEnd - issue.eStart;

    $("#issues").append(
        $("<div></div>").html(issue.description).addClass("issue-bar").attr({
            title: issue.description + " since " + issue.eStart
        }).css({
            "width": issue.eDuration / duration * 100 + "%",
            "margin-left": (issue.eStart - start) / duration * 100 + "%"
        }).click(function() {
            window.location.href = "issue/" + issue.id + "/overview";
        })
    );
});

function merge(segments) {
    var merged = [];
    segments.forEach(function(segment) {
        var shortened = { eStart: segment.eStart, eEnd: segment.eEnd };
        if (!merged.length) {
            merged.push(shortened);
        } else if (segment.eStart <= merged[merged.length-1].eEnd) {
            merged[merged.length-1].eEnd = new Date(
                Math.max(segment.eEnd, merged[merged.length-1].eEnd)
            );
        } else {
            merged.push(shortened);
        }
    });
    return merged;
}

function complement(start, end, segments) {
    var points = [];
    segments.forEach(function(segment) {
        points.push(segment.eStart);
        points.push(segment.eEnd);
    });
    if (points[0] == start) {
        points.shift();
    } else {
        points.unshift(start);
    }
    if (points[points.length-1] == end) {
        points.pop();
    } else {
        points.push(end)
    }
    var results = [];
    for (i=0; i<points.length; i+=2) {
        results.push({
            eStart: points[i],
            eEnd: points[i+1]
        });
    }
    return results;
}

function intersect(segments1, segments2) {
    // arguments should not contain overlapping segments
    // within itself (call merge first)
    var points = []
    segments1.forEach(function(s) {
        points.push({ type: "s1", value: s.eStart });
        points.push({ type: "e1", value: s.eEnd });
    });
    segments2.forEach(function(s) {
        points.push({ type: "s2", value: s.eStart });
        points.push({ type: "e2", value: s.eEnd });
    });
    points.sort(function(a, b){ return a.value - b.value; });
    var intersections = [];
    var acc1 = null;
    var acc2 = null;
    var acc = false;
    points.forEach(function(point) {
        if (point.type == "s1") {
            acc1 = point;
        } else if (point.type == "s2") {
            acc2 = point;
        } else if (point.type == "e1") {
            acc1 = null;
        } else if (point.type == "e2") {
            acc2 = null;
        }
        if (!acc && acc1 && acc2) {
            intersections.push(point.value);
            acc = true;
        } else if (acc && !(acc1 && acc2)) {
            intersections.push(point.value);
            acc = false;
        }
    });
    var results = [];
    for (i=0; i<intersections.length; i+=2) {
        results.push({
            eStart: intersections[i],
            eEnd: intersections[i+1]
        });
    }
    return results;
}

var uptimesDuration = uptimes.reduce(function(acc, elem) {
    return acc + (elem.eEnd - elem.eStart);
}, 0);

var issuesMerge = merge(issues);
var issuesDuration = issuesMerge.reduce(function(acc, elem) {
    return acc + (elem.eEnd - elem.eStart);
}, 0);

var affectingIssuesMerge = merge(issues.filter(function(elem) {
    return elem.affectUptime;
}));
var affectingIssuesDuration = affectingIssuesMerge.reduce(function(acc, elem) {
    return acc + (elem.eEnd - elem.eStart);
}, 0);

var affectMerge = intersect(
    affectingIssuesMerge,
    complement(start, end, uptimes)
);
var affectDuration = affectMerge.reduce(function(acc, elem) {
    return acc + (elem.eEnd - elem.eStart);
}, 0);

function roundTo(places, value) {
    return Math.round(value * Math.pow(10, places)) / Math.pow(10, places);
}

$("#summary").append($("<div></div>").html(
    "Raw: "
    + roundTo(2, uptimesDuration/1000/60/60/24)
    + " days ("
    + roundTo(1, uptimesDuration/1000/60/60)
    + " hours, "
    + roundTo(1, uptimesDuration / duration * 100)
    + "%)"
));

//$("#summary").append($("<div></div>").html(
//    "Duration with issues: "
//    + roundTo(2, issuesDuration/1000/60/60/24)
//    + " days ("
//    + roundTo(1, issuesDuration/1000/60/60)
//    + " hours)"
//));
//
//$("#summary").append($("<div></div>").html(
//    "Duration with affecting issues: "
//    + roundTo(2, affectingIssuesDuration/1000/60/60/24)
//    + " days ("
//    + roundTo(1, affectingIssuesDuration/1000/60/60)
//    + " hours)"
//));
//
//$("#summary").append($("<div></div>").html(
//    "Uptime affected by issues: "
//    + roundTo(2, affectDuration/1000/60/60/24)
//    + " days ("
//    + roundTo(1, affectDuration/1000/60/60)
//    + " hours)"
//));

$("#summary").append($("<div></div>").html(
    "Discounted: "
    + roundTo(2, (uptimesDuration + affectDuration)/1000/60/60/24)
    + " days ("
    + roundTo(2, (uptimesDuration + affectDuration)/1000/60/60)
    + " hours, "
    + roundTo(1, (uptimesDuration + affectDuration) / duration * 100)
    + "%)"
));

cvExceedsDictionary = {};
cvExceeds.forEach(function(cve) {
    if (cvExceedsDictionary[cve.cv]) {
        cvExceedsDictionary[cve.cv].push(cve);
    } else {
        cvExceedsDictionary[cve.cv] = [cve];
    }
    // Mark cv as has constrained in the all cvs collection
    _.find(cvs, function(cv) { return cv.id == cve.cv; }).constrained = true;
});
cvExceedsGroupedEconomic = [];
cvExceedsGroupedConstraint = [];
cvExceedsGroupedProtective = [];
for (var cvid in cvExceedsDictionary) {
    cvExceedsDictionary[cvid].forEach(function (exc) {
          exc.start = new Date(exc.start);
          exc.end = new Date(exc.end);
          exc.duration = exc.end - exc.start;
          exc.eStart = new Date(Math.max(exc.start, start));
          exc.eEnd = new Date(Math.min(exc.end, end));
          exc.eDuration = exc.eEnd - exc.eStart;
          exc.left = (exc.eStart - start) / duration * 800;
          exc.width = exc.eDuration / duration * 800;
    });
    switch (_.find(cvs, function(cv) { return cv.id == cvid }).category) {
        case "EconomicCv":
            cvExceedsGroupedEconomic.push(cvExceedsDictionary[cvid]);
            break;
        case "ConstraintCv":
            cvExceedsGroupedConstraint.push(cvExceedsDictionary[cvid]);
            break;
        case "ProtectiveCv":
            cvExceedsGroupedProtective.push(cvExceedsDictionary[cvid]);
            break;
    }
}
function renderCvBar(list, divId) {
    var row = d3.select(divId).selectAll(".cv-exceed")
        .data(list).enter().append("svg")
            .attr("class", "cv-exceed")
            .attr("width", "800")
            .attr("height", "22")
            .attr("data-cvid", function(d) { return d[0].cv; });
    row.selectAll("rect").data(function(d) { return d || []; }) 
        .enter().append("rect")
            .attr("x", function(exc) { return exc.left; })
            .attr("width", function(exc) { return exc.width; })
            .attr("height", 22)
            .attr("fill", "yellow");
    row.append("text")
        .attr("x", 400)
        .attr("y", 18)
        .attr("font-size", 16)
        .attr("text-anchor", "middle")
        .text(function(d) { return _.findWhere(cvs, {id: d[0].cv}).name; });
}
renderCvBar(cvExceedsGroupedEconomic, "#cv-exceeds-economic");
renderCvBar(cvExceedsGroupedConstraint, "#cv-exceeds-constraint");
renderCvBar(cvExceedsGroupedProtective, "#cv-exceeds-protective");

$(".cv-exceed").click(function(e) {
    var id = $(e.currentTarget).attr("data-cvid");
    window.location.href = "cv/" + id + "/trend";
});
