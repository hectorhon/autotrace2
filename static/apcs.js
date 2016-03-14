var duration = end - start;

uptimess.forEach(function(uptimes) {
    console.log(uptimes);
    uptimes.forEach(function(uptime) {
        console.log(uptime);
        uptime.start = new Date(uptime.start);
        uptime.end = new Date(uptime.end);
        uptime.duration = uptime.end - uptime.start;
    
        uptime.eStart = new Date(Math.max(uptime.start, start));
        uptime.eEnd = new Date(Math.min(uptime.end, end));
        uptime.eDuration = uptime.eEnd - uptime.eStart;
    
        $("#uptimes-" + uptime.apc).append(
            $("<div></div>").html("&nbsp").addClass("uptime-bar").css({
                "width": uptime.eDuration / duration * 100 + "%",
                "margin-left": (uptime.eStart - start) / duration * 100 + "%",
                "top" : 0
            })
        );
    });
});
