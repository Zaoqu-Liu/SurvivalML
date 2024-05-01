// create new input binding
var shinyLink = new Shiny.InputBinding();

// define methods
$.extend(shinyLink, {
    find: function(scope) {
        return $(scope).find("a.shiny__link");
    },
    initialize: function(el) {
        $(el).on("click", function(e) {
            e.preventDefault();
            
            // extract destination and find matching link
            var to = $(el).attr("href");
            var target = $(`a[data-value=${to}]`);

            // does the link exist?
            if (target.length) {

                // if the parent ul of the matching link has the class `.navbar-nav`,
                // then click the link. Links with these classes are
                // located in the navbar element.
                if ($(target).parent().parent().hasClass("navbar-nav")) {
                    $(target).click();
                    window.scrollTo(0, 0,);
                }
                
                // if the parent ul of the matching link has the class `.nav-tabs`,
                // then this indicates that the link is part of of a tabSetPanel
                // inside a tabPanel. This means, that parent tabPanel must
                // be found and activated before activating the tabPanel.
                if ($(target).parent().parent().hasClass("nav-tabs")) {
                    
                    // find the nearest .tab-pane and extract `data-value`
                    var val = $(target).closest("div.tab-pane").data("value");
                    var parentLink = $(`a[data-toggle="tab"][data-value="${val}"]`);

                    // activate parent (if not already active)
                    if (!parentLink.parent("li").hasClass("active")) {
                        parentLink.click();
                    }

                    // activate destination tab
                    target.click();
                    window.scrollTo(0, 0,);

                }
                
            } else {
                console.error("No matching link found. Is the destination correct?");
            }
        });
    }
});


// register
Shiny.inputBindings.register(shinyLink);
