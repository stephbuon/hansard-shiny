document.getElementById("most_similar-download_similarity").onclick = function() {
  var gd = document.getElementById("most_similar-most_similar");
  Plotly.Snapshot.toImage(gd, {format: "png"}).once("success", function(url) {
    var a = window.document.createElement("a");
    a.href = url; 
    a.type = "image/png";
    a.download = "plot.png";
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a); }); }
