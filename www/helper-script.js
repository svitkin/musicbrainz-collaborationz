$(document).keyup(function(event) {
    if ($("#artistName").is(":focus") && (event.key == "Enter")) {
        $("#makeTable").click();
    }
});

function searchArtistFromTable(artistElement) {
    var collabName = $(artistElement).text();
    $("#artistName").val(collabName);
    Shiny.setInputValue("artistName", collabName);
    $("#makeTable").click();
}
