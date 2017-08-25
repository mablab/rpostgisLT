$(document).on("keydown",
    function (e) {
        if(e.which == 37) {
            Shiny.onInputChange("p", new Date() );
        } else if (e.which == 39) {
            Shiny.onInputChange("n", new Date() );
        }
    }
);
