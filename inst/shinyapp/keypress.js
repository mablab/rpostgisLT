$(document).on("keydown",
    function (e) {
        if(e.which == 37) {
            Shiny.onInputChange("b", new Date() );
        } else if (e.which == 39) {
            Shiny.onInputChange("n", new Date() );
        }
    }
);
