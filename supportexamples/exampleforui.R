#https://semantic-ui.com/elements/icon.html#user-actions codes

demo <- function(code) {
  div(class = "ui raised segment",
      code,
      div(style = "width: 100%; height:10px"),
      highlight(formatR::tidy_source(width.cutoff = 40, text = deparse(substitute(code)))$text.tidy)
  )
}

input <- function(class = "ui input", style = "", type = "text", name = "", placeholder = "") {
  div(class = class, style = style,
      tags$input(type = type, name = name, placeholder = placeholder)
  )
}

jsCode <- "
$('.accordion').accordion({selector: {trigger: '.title .icon'}}).accordion('close');
$('.ui.dropdown').dropdown({});
$('.rating').rating('setting', 'clearable', true);
"

header <- function() {
  div(
    h1(class="ui header", id="header", "Headers"),
    demo(h1(class="ui header", "First header")),
    demo(h2(class="ui header", "Second header")),
    demo(h2(class="ui icon header",
            uiicon("settings"),
            div(class="content", "Account Settings",
                div(class="sub header", "Manage your account")))),
    demo(h2(class="ui header", uiicon("plug"), div(class="content", "Second header"))),
    demo(h2(class="ui header", uiicon("settings"),
            div(class="content", "Second header",
                div(class="sub header", "Manage preferences"))))
  )
}
button <- function() {
  div(
    h1(class="ui header", id="button", "Button"),
    demo(div(class = "ui raised segment", "Raised segment")),
    demo(div(class = "ui button", "Button")),
    demo(div(class = "ui basic button", "Basic button")),
    demo(div(class = "ui basic button", uiicon("user"), "Icon button")),
    demo(div(class = "ui olive basic button", uiicon("user"), "Color icon button")),
    demo(div(class = "ui animated button", tabindex="0",
             div(class="visible content", "Next"),
             div(class="hidden content", "Step"))),
    demo(div(class = "ui labeled button", tabindex="0",
             div(class="ui button", uiicon("heart"), "Like"),
             a(class="ui basic label", "2,048"))),
    demo(div(class = "ui mini button", "Mini")),
    demo(div(class = "ui tiny button", "Tiny")),
    demo(div(class = "ui small button", "Small")),
    demo(div(class = "ui medium button", "Medium")),
    demo(div(class = "ui large button", "Large")),
    demo(div(class = "ui big button", "Big")),
    demo(div(class = "ui huge button", "Huge")),
    demo(div(class = "ui massive button", "Massive")),
    demo(div(class = "ui red button", "Red")),
    demo(div(class = "ui orange button", "Orange")),
    demo(div(class = "ui yellow button", "Yellow")),
    demo(div(class = "ui olive button", "Olive")),
    demo(div(class = "ui green button", "Green")),
    demo(div(class = "ui teal button", "Teal")),
    demo(div(class = "ui blue button", "Blue")),
    demo(div(class = "ui violet button", "Violet")),
    demo(div(class = "ui purple button", "Purple")),
    demo(div(class = "ui pink button", "Pink")),
    demo(div(class = "ui brown button", "Brown")),
    demo(div(class = "ui grey button", "Grey")),
    demo(div(class = "ui black button", "Black"))
  )
}
divider <- function() {
  div(
    h1(class="ui header", id="divider", "Divider"),
    demo(div(class="ui divider")),
    demo(div(class="ui horizontal divider", "Or")),
    demo(div(class="ui horizontal divider", uiicon("tag"), "Description"))
  )
}
uiinput <- function() {
  div(
    h1(class="ui header", id="input", "Input"),
    demo(div(class="ui input", input(placeholder="Search..."))),
    demo(div(class="ui icon input", input(placeholder="Search..."), uiicon("search"))),
    demo(div(class="ui icon input", input(placeholder="Search..."), uiicon("circular link search"))),
    demo(div(class="ui right labeled input",
             tags$input(type="text", placeholder="Enter weight..."),
             div(class="ui basic label" ,"kg")))
  )
}
breadcrumb <- function() {
  div(
    h1(class="ui header", id="breadcrumb", "Breadcrumb"),
    demo(div(class="ui breadcrumb",
             a(class="section", "Home"),
             div(class="divider", "/"),
             a(class="section", "Store"),
             div(class="divider", "/"),
             a(class="section", "T-shirts"))),
    demo(div(class="ui breadcrumb",
             a(class="section", "Home"),
             uiicon("right angle divider"),
             a(class="section", "Store"),
             uiicon("right angle divider"),
             a(class="section", "T-shirts"))),
    demo(div(class="ui huge breadcrumb",
             a(class="section", "Home"),
             uiicon("right angle divider"),
             a(class="section", "Store"),
             uiicon("right angle divider"),
             a(class="section", "T-shirts")))
  )
}
accordion <- function() {
  div(
    h1(class="ui header", id="accordion", "Accordion"),
    demo(div(class="ui styled accordion",
             div(class="active title", uiicon('dropdown icon'), "What is dog?"),
             div(class="active content", p("A dog is a type of domesticated animal. Known for its loyalty and faithfulness, it can be found as a welcome guest in many households across the world.")),
             div(class="title", uiicon('dropdown icon'), "What kinds of dogs are there?"),
             div(class="content", p("There are many breeds of dogs. Each breed varies in size and temperament. Owners often select a breed of dog that they find to be compatible with their own lifestyle and desires from a companion."))
    )
    )
  )
}
grid <- function() {
  div(
    h1(class="ui header", id="grid", "Grid"),
    demo(div(class="ui grid",
             div(class="four wide column", "Column"),
             div(class="four wide column", "Column"),
             div(class="four wide column", "Column"),
             div(class="four wide column", "Column"))),
    demo(div(class="ui stackable four column grid",
             div(class="column", "Column"),
             div(class="column", "Column"),
             div(class="column", "Column"),
             div(class="column", "Column")))
  )
}
card <- function() {
  div(
    h1(class="ui header", id="card", "Card"),
    demo(
      div(class="ui card",
          div(class="content",
              div(class="right floated meta", "14h"),
              img(class="ui avatar image", src="images/elliot.jpg"),
              "Elliot"
          ),
          div(class="image", img(src="images/wireframe.png")),
          div(class="content",
              span(class="right floated", uiicon("heart outline like"), "17 likes"),
              uiicon("comment"),
              "3 comments"
          ),
          div(class="extra content",
              div(class="ui large transparent left icon input",
                  uiicon("heart ouline"),
                  tags$input(type="text", placeholder ="Add Comment...")
              )
          )
      )
    )
  )
}
uilabel <- function() {
  div(
    h1(class="ui header", id="label", "Label"),
    demo(h1(class="ui header", "First header"))
  )
}

rating <- function() {
  div(
    h1(class="ui header", id="rating", "Rating"),
    demo(div(class = "ui star rating"))
  )
}
uilist <- function() {
  div(
    h1(class="ui dividing header", id="list", "List"),
    demo(div(class="ui list", c("Apples", "Pears", "Oranges") %>% purrr::map(~ div(class="item", .)))),
    demo(div(class="ui list", c("Apples", "Pears", "Oranges") %>% purrr::map(~
                                                                               div(class="item", uiicon('users'), div(class="content", .))))),
    demo(div(class="ui relaxed divided list", c("Apples", "Pears", "Oranges") %>% purrr::map(~
                                                                                               div(class="item",
                                                                                                   uiicon('large github middle aligned'),
                                                                                                   div(class="content",
                                                                                                       a(class="header", "Hello"),
                                                                                                       div(class="description", .))
                                                                                               ))))
  )
}
section <- function() {
  div(
    h1(class="ui dividing header", id="section", "Section"),
    demo(h1(class="ui header", "First header"))
  )
}





#Dropdown function not included in the package
dropdown <- function(name, choices, choices_value = choices, default_text = 'Select', value = NULL) {
  unique_dropdown_class <- paste0('dropdown_name_', name)
  
  tagList(
    div(class = paste("ui selection fluid dropdown", unique_dropdown_class),
        shiny_text_input(name, shiny::tags$input(type = "hidden", name = name), value = value),
        uiicon("dropdown"),
        div(class = "default text", default_text),
        div(class = "menu",
            purrr::map2(choices, choices_value, ~ div(class = "item", `data-value` = .y, .x))
        )
    ),
    tags$script(paste0("$('.ui.dropdown.", unique_dropdown_class, "').dropdown().dropdown('set selected', '", value,"');"))
  )
}
