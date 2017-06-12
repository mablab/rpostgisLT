/*
$(document).keyup(function(event) {
    if ($("#text").is(":focus") && (event.keyCode == 13)) {
        $("#nextButton").click();
    }
});


$(document).keyup(function(event){
    if(event.keyCode == 78){
        $("#nextButton").click();
    }
});

$(document).keyup(function(event){
    if(event.keyCode == 66){
        $("#backButton").click();
    }
});
*/

// code included inside $(document).ready() will only run once the page is ready for JavaScript code to execute
$(document).ready(function() {
    //var n = 1;
    //var b = 1;
  // create a click handler which listens for a click on the element with id equal to RStudio
  $(document).on("keydown", function(e){
    if (e.which == 78) {
    // send message to Shiny
    //Shiny.onInputChange("n", n);
    $("#nextButton").click();
    }
  });
  $(document).on("keydown", function(e){
    if (e.which == 66) {
    // send message to Shiny
    //(Shiny.onInputChange("b", b);
    $("#backButton").click();
    }
  });
});