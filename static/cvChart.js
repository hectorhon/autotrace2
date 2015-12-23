start = new Date(start*1000);
end = new Date(end*1000);
var dataArray = [];
for (tagName in data) {
    data[tagName].forEach(function(point) {
        point.t = new Date(point.t * 1000);
    });
    dataArray.push({
        tagName: tagName,
        points: data[tagName],
        minY: d3.min(data[tagName], function(d) { return d.v; }),
        maxY: d3.max(data[tagName], function(d) { return d.v; }),
    });
}

var margin = { top: 50, right: 50, bottom: 50, left: 50 };
var width = 800 - margin.left - margin.right;
var height = 400 - margin.top - margin.bottom;

var x = d3.time.scale()
.range([0, width])
.domain([start, end]);

var minY = d3.min(dataArray, function(d) { return d.minY; });
var maxY = d3.max(dataArray, function(d) { return d.maxY; });
var spanY = maxY - minY;
var y = d3.scale.linear()
.range([height, 0])
.domain([minY - spanY * 0.2, maxY + spanY * 0.2]);

var xAxis = d3.svg.axis().scale(x).orient("bottom");
var yAxis = d3.svg.axis().scale(y).orient("left");

var line = d3.svg.line().x(function(d) { return x(d.t); })
                        .y(function(d) { return y(d.v); });

var svg = d3.select("#chart")
.append("svg")
.attr("width", width + margin.left + margin.right)
.attr("height", height + margin.top + margin.bottom)
.append("g")
.attr("transform", "translate("+margin.left+","+margin.top+")")

svg.append("g")
.attr("class", "axis")
.attr("transform", "translate(0,"+height+")")
.call(xAxis);

svg.append("g")
.attr("class", "axis")
.call(yAxis);

svg.selectAll(".line").data(dataArray).enter()
.append("path").datum(function(d) { return d.points; })
.attr("class", "line")
.attr("d", line);
