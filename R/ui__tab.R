ui__tab__home <- function(ses) {
  trace_func_entry("ui__tab__home")
  # The Homepage must answer the five W-questions:
  # 1. What is this site?
  # 2. What can I do here?
  # 3. What do they have here?
  # 4. Why should I be here and not somewhere else?
  # 5. Where to start?
  # Ideas:
  # 1. Carousel of DLBCL, machine learning, etc. and overlay text "Predict,
  #    Compare, Explain - use machine learning methods to make prediction for
  #    DLBLC. Accompanied with intuitive visualizations and explanations".
  #    Figure caption should contain links to "Predict", "Compare", "Explain".
  # 2. Page Header with
  #    1. A tagline (h1): "Making machine learning interpretable"
  #    2. A welcome blurb (h2), i.e., a terse description of the site,
  #       displayed in a prominent block on the Home page: "IMLUI provides a
  #       grafical user interface to apply and explain machine learning methods
  #       to diffuse large b-cell (DLBCL) data"
  #    3. Learn more (h2), i.e., multiple images or videos with short
  #       texts like
  #       1. VID: "[Get Stared](lnk). Short introduction video explaining how to get the most out of the IML UI for DLBCL."
  #       2. IMG: "[Compare](link) multiple models on the same set of samples."
  #       3. IMG: "[Predict](lnk) new samples. Choose from more than 15 different models published from 1998-2022."
  tags$div(
    class = "container-fluid",
    tags$h1(
      "Interpret Machine Learning Models for DLBCL"
    ),
    tags$p(
      "Apply methods from the field of Intepretable Machine Learning (IML) and
      Explainable Artifical Intelligence (XAI) to existing molecular models for
      Diffuse Large B-Cell Lymphoma (DLBCL). Obtain predictions for new patients. Compare multiple models on the same dataset or apply one model to multiple datasets."
    ),
    # ui__tab__home_carousel(ses),
    # tags$hr(),
    tags$h2("Understand Model Predictons"),
    fluidRow(
      column(width = 6, ui__img__fep_1200x400),
      column(
        width = 6,
        tags$p(
          'Use ',
          html__ia(
            href_id = "tab__mdl__feature_effects",
            text = "Feature Effect Plots"
          ),
          ' to visualize the effect of each feature on a prediction. The big orange line in each column shows the value of the feature depicted in that column for the current sample of interest. The red/blue lines show the feature value for the n samples with most unfavorable/favorable prediction score. The width of each column is proportional to the importance of the feature for the model prediction. The color of the background gives the class of the sample and therefore, implicitly, also the sign of the feature weight: if an increase in feature value causes the orange line to move into a red/blue background, the feature has negative/positive weight.'
        )
      )
    ),
    tags$hr(),
    # tags$h2("Make Predictions for new patients"),
    # fluidRow(
    #   column(width = 6, tags$p(
    #     "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc rhoncus
    #     nisl eu massa condimentum, sit amet pellentesque sapien fermentum. Etiam
    #     non cursus turpis. Mauris pellentesque libero sit amet justo molestie
    #     accumsan. Pellentesque turpis nisl, rhoncus blandit orci sed, malesuada
    #     cursus lorem. Pellentesque viverra diam non hendrerit ornare. Praesent
    #     aliquam sem et dui volutpat, ullamcorper cursus odio auctor. Class
    #     aptent taciti sociosqu ad litora torquent per conubia nostra, per
    #     inceptos himenaeos. Praesent eget molestie sapien, eu rhoncus risus.
    #     Etiam sagittis ex dui, vel lobortis metus aliquet quis. Integer in
    #     ullamcorper velit. Maecenas turpis magna, placerat at justo ut,viverra
    #     fringilla ligula")
    #   ),
    #   column(width = 6, ui__img__pred_1200x400)
    # ),
    # tags$hr(),
    # tags$h2("Compare Models"),
    # fluidRow(
    #   column(width = 6, ui__img__msd_1200x400),
    #   column(width = 6, tags$p(
    #     "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc rhoncus
    #     nisl eu massa condimentum, sit amet pellentesque sapien fermentum. Etiam
    #     non cursus turpis. Mauris pellentesque libero sit amet justo molestie
    #     accumsan. Pellentesque turpis nisl, rhoncus blandit orci sed, malesuada
    #     cursus lorem. Pellentesque viverra diam non hendrerit ornare. Praesent
    #     aliquam sem et dui volutpat, ullamcorper cursus odio auctor. Class
    #     aptent taciti sociosqu ad litora torquent per conubia nostra, per
    #     inceptos himenaeos. Praesent eget molestie sapien, eu rhoncus risus.
    #     Etiam sagittis ex dui, vel lobortis metus aliquet quis. Integer in
    #     ullamcorper velit. Maecenas turpis magna, placerat at justo ut,viverra
    #     fringilla ligula"
    #   ))
    # ),
    # tags$hr(),
    tags$h2("Validate Datasets"),
    fluidRow(
      column(
        width = 6,
        tags$p(
          'Use 2D embeddings like ',
          html__ia(href_id = "tab__datasets__pca", text = "PCA"),
          ', ',
          html__ia(href_id = "tab__datasets__umap", text = "UMAP"),
          ' or ',
          html__ia(href_id = "tab__datasets__tsne", text = "t-SNE"),
          ' to ensure that the distribution of new, uploaded datasets fits the distribution of the data the model was trained and tested on. If the new dataset is too different, the model predictions might not be as accurate for new patients, as the plain prediction scores might suggest. Another visualizations that might be helpful to validate the distributon of a dataset are ',
          html__ia(href_id = "tab__datasets__msd", text = "Mean-SD-Plots"),
          ' and classc',
          html__ia(href_id = "tab__mdl__predictions", text = "Headmaps"),
          '.'
          )
      ),
      column(width = 6, ui__img__msd_1200x400)
    ),
    tags$hr(),
    tags$h2("Host on your own PC"),
    tags$p(
      "This site is hosted through the IMLUI R package. That means, you can easily host you own IML-Website (e.g. with a focus on different types of
      datasets) by just installing and running",
      tags$a("R", href = "https://cran.r-project.org/"),
      "plus the",
      tags$a("imlui", href = "https://github.com/toscm/imlui"),
      "package. For more details see",
      tags$a(
        "https://github.com/toscm/imlui",
        href = "https://github.com/toscm/imlui"
      ),
      "."
    )
  )
}


ui__tab__models <- function(ses) {
  trace_func_entry()
  tags$div(class = "container-fluid", "Models")
  tags$div(
    tags$p("Methods for analyzing models:"),
    tags$ol(
      tags$li(html__ia("tab__mdl__predictions", "Predictions")),
      tags$li(html__ia("tab__mdl__features", "Feature Coefficients")),
      tags$li(html__ia("tab__mdl__feature_effects", "Feature Effect Plots")),
      tags$li(html__ia("tab__mdl__survival_curves", "Survival Curves"))
    )
  )
}


ui__tab__datasets <- function(ses) {
  tags$div(class = "container-fluid", "Datasets")
}


ui__tab__settings <- function(ses) {
  trace_func_entry("ui__tab__settings")
  tags$div(
    class = "container-fluid",
    tags$h1("Settings"),
    tags$ul(
      tags$li(html__ia("tab__settings__users", "Users")),
      tags$li(html__ia("tab__settings__models", "Models")),
      tags$li(html__ia("tab__settings__datasets", "Datasets")),
      tags$li(html__ia("tab__settings__papers", "Papers")),
      tags$li(html__ia("tab__settings__settings", "Settings")),
      tags$li(html__ia("tab__settings__appstate", "Appstate")),
      tags$li(html__ia("tab__settings__samples", "Samples")),
      tags$li(html__ia("tab__settings__modeltypes", "Modeltypes")),
      tags$li(html__ia("tab__settings__datatypes", "Datatypes")),
      tags$li(html__ia("tab__settings__methods", "Methods")),
      tags$li(html__ia("tab__settings__platforms", "Platforms")),
      tags$li(html__ia("tab__settings__mapping_users_models", "Mapping Users Models")),
      tags$li(html__ia("tab__settings__mapping_users_sessions", "Mapping Users Sessions")),
      tags$li(html__ia("tab__settings__mapping_users_groups", "Mapping Users Groups")),
      tags$li(html__ia("tab__settings__mapping_users_resources", "Mapping Users Resources")),
      tags$li(html__ia("tab__settings__mapping_users_datasets", "Mapping Users Datasets")),
      tags$li(html__ia("tab__settings__mapping_groups_models", "Mapping Groups Models")),
      tags$li(html__ia("tab__settings__mapping_groups_datasets", "Mapping Groups Datasets")),
      tags$li(html__ia("tab__settings__mapping_groups_resources", "Mapping Groups Resources")),
      tags$li(html__ia("tab__settings__mapping_papers_models", "Mapping Papers Models")),
      tags$li(html__ia("tab__settings__mapping_papers_datasets", "Mapping Papers Datasets")),
      tags$li(html__ia("tab__settings__mapping_datasets_features", "Mapping Datasets Features")),
      tags$li(html__ia("tab__settings__mapping_models_features", "Mapping Models Features"))
    )
  )
}


ui__tab__debug <- function(ses) {
  ui <- tags$div("Debug")
  return(ui)
}


ui__tab__login <- function(ses) {
  trace_func_entry()
  # TODO: make width responsive for small devices
  div(
    id = "login_panel",
    # tags$h1("Login"),
    wellPanel(
      textInput("login_user_name", tagList(icon("user"), "UserID or Email")),
      passwordInput("login_password", tagList(icon("unlock"), "Password")),
      div(
        style = "text-align: center;",
        actionButton(
          inputId = "login_button",
          label = "Sign in",
          class = "btn btn-primary btn-block"
        )
      ),
      div(
        id = "login_error",
        style = "display: none;",
        tags$p(
          "Login failed!",
          style = "color: red; font-weight: bold; padding-top: 5px;",
          class = "text-center"
        )
      )
    ),
    wellPanel(
      div(
        style = "text-align: left;",
        tags$div(
          tagList(
            icon("right-to-bracket"),
            tags$b("Sign in with")
          ),
          style = "text-align: left;"
        ),
        # actionButton(
        #   inputId = "login_button_auth_spang_lab",
        #   label = tagList(
        #     img(
        #       src = "imlui/assets/png/spang-lab-logo-64x64.png",
        #       alt = "spang-lab-logo.png",
        #       style = "height: 1.5em;"
        #     ),
        #     "Spang Auth"
        #   ),
        #   class = "btn-block"
        # ),
        # actionButton(
        #   inputId = "login_button_gitlab_spang_lab",
        #   label = tagList(
        #     img(
        #       src = "imlui/assets/png/spang-lab-logo-64x64.png",
        #       alt = "spang-lab-logo.png",
        #       style = "height: 1.5em;"
        #     ),
        #     "Spang Gitlab"
        #   ),
        #   class = "btn-block"
        # ),
        # actionButton(
        #   inputId = "login_button_google",
        #   label = tagList(
        #     img(
        #       src = "imlui/assets/png/google-logo-48x48.png",
        #       alt = "google-logo.png",
        #       style = "height: 1.5em;"
        #     ),
        #     "Google"
        #   ),
        #   class = "btn-block"
        # ),
        # actionButton(
        #   inputId = "login_button_gitlab",
        #   label = tagList(
        #     img(
        #       src = "imlui/assets/png/gitlab-icon-rgb.svg",
        #       alt = "gitlab-icon.svg",
        #       style = "height: 1.5em;"
        #     ),
        #     "Gitlab"
        #   ),
        #   class = "btn-block"
        # ),
        actionButton(
          inputId = "login_button_github",
          label = tagList(
            img(
              src = "imlui/assets/png/github-mark-32px.png",
              alt = "github-mark.png",
              style = "height: 1.5em;"
            ),
            "GitHub"
          ),
          class = "btn-block"
        )
      )
    )
  )
}


ui__tab__user <- function(ses) {
  trace_func_entry()
  tags$div(class = "container-fluid", "User")
}


ui__tab__contact <- function(ses) {
  trace_func_entry("ui__tab__contact")
  tags$div(
    class = "container-fluid",
    html__p(
      "Tobias Schmidt", html__br(),
      "Department of Statistical Bioinformatics", html__br(),
      "University of Regensburg", html__br(),
      "Am BioPark 9", html__br(),
      "93053 Regensburg", html__br(),
      "Germany", html__br(),
      html__a(href = "tobias2.schmidt@ur.de", "tobias2.schmidt@ur.de"),
      html__br(),
      "+49 941 943 5053"
    )
  )
}


ui__tab__legal_notice <- function(ses) {
  trace_func_entry()
  tags$div(class = "container-fluid",

      html__h3("Responsible for the content"),

      html__p(
        "Tobias Schmidt", html__br(),
        "Department of Statistical Bioinformatics", html__br(),
        "University of Regensburg", html__br(),
        "Am BioPark 9", html__br(),
        "93053 Regensburg", html__br(),
        "Germany", html__br(),
        html__a(href = "tobias2.schmidt@ur.de", "tobias2.schmidt@ur.de"), html__br(),
        "+49 941 943 5053"
      ),

      html__h3("Disclaimer"),

      html__h4("Liability for Content:"),
      html__p("The content on this website is subject to general law in accordance with Section 7 (1) of the German Telemedia Act (TMG). However, according to Sections 8 to 10 TMG, the website is not obligated to monitor or investigate information transmitted or stored by third parties that may indicate unlawful activity."),

      html__h4("Liability for Links:"),
      html__p("This website may contain links to external third-party websites. The content on these linked websites is the responsibility of the respective provider or operator. The linked websites were checked for possible legal violations at the time of linking. Unlawful content was not apparent at the time of linking. However, a permanent control of the content of the linked websites is not reasonable without concrete evidence of a violation of the law. If any legal violations are brought to our attention, such links will be removed immediately."),

      html__h4("Copyright:"),
      html__p("The content and works on these pages created by the website operator are subject to German copyright law. The reproduction, editing, distribution, and any kind of use outside the limits of copyright law require the written consent of the respective author or creator. Downloads and copies of this website are only permitted for private, non-commercial use. Insofar as the content on this website was not created by the operator, the copyrights of third parties are respected. In particular, third-party content is identified as such. If any copyright infringement is brought to our attention, such content will be removed immediately."),

      html__h4("Data Protection:"),
      html__p("The use of this website is generally possible without providing personal data. If personal data is collected on this website (for example, name, address or email address), this is always done on a voluntary basis. This data will not be passed on to third parties without the express consent of the user. Please note that data transmission over the internet (e.g. communication by email) may be subject to security breaches. A complete protection of the data against access by third parties is not possible.")
  )
}


ui__tab__privacy_policy <- function(ses) {
  trace_func_entry("ui__tab__privacy_policy")
  tags$div(
    class = "container-fluid",
      html__h3("Cookies"),
      html__p("This website does not use cookies except a session cookie required for user login"),

      html__h3("Server Log"),
      html__p("The web server keeps a log of all requests, with the following data:"),
      html__ul(
        html__li("Date and Time of the request"),
        html__li("Request arguments"),
        html__li("ID of currently logged in user")
      ),

      html__h3("Web Analytics / Other Tracking"),
      html__p("There are no other tracking methods."),

      html__h3("User Login"),
      html__p("Users can sign in and give personal information to this application."),
      html__p(
        "Login can also be handled through the OAuth2.0 protocol with ",
        html__a(href = "https://github.de",  "https://github.de"),
        " as identity provider. In this case privacy policy of ",
        html__a(href = "https://github.de",  "https://github.de"),
        " applies as well."
      ),

      html__h3("Privacy Contact"),
      html__p(
        "Institute of functional genomics",
        html__br(),
        "Statistical Bioinformatics",
        html__br(),
        "Am BioPark 9",
        html__br(),
        "93053 Regensburg",
        html__br(),
        "Germany",
        html__br(),
        html__a(
          href = "sekretariat.genomik@ur.de",
          "sekretariat.genomik@ur.de"
        ),
        html__br(),
        "+49 941 943 5053"
      ),

      html__p(
        "We are part of the University of Regensburg. The contact for the university can be found ",
        html__a(href = "https://www.uni-regensburg.de/kontakt/startseite/index.html", "here")
      )
  )
}


ui__tab__home_carousel <- function(ses) {
  tags$div(
    `id` = "carouselExampleIndicators",
    `class` = "carousel slide",
    `data-ride` = "carousel",
    tags$ol(
      class = "carousel-indicators",
      tags$li(
        `data-target` = "#carouselExampleIndicators",
        `data-slide-to` = "0",
        class = "active"
      ),
      tags$li(
        `data-target` = "#carouselExampleIndicators",
        `data-slide-to` = "1"
      ),
      tags$li(
        `data-target` = "#carouselExampleIndicators",
        `data-slide-to` = "2"
      )
    ),
    div(
      class = "carousel-inner", role = "listbox",
      div(
        class = "item active",
        ui__img__pred2_1200x400,
        div(
          class = "carousel-caption",
          "Understand Model Predictions"
        )
      ),
      div(
        class = "item",
        ui__img__pred_1200x400,
        div(
          class = "carousel-caption",
          "Make Predictions for new patients"
        )
      ),
      div(
        class = "item",
        ui__img__msd_1200x400,
        div(
          class = "carousel-caption",
          "Compare Different Models and Datasets"
        )
      )
    ),
    tags$a(
      class = "carousel-control-prev",
      href = "#carouselExampleIndicators",
      role = "button",
      `data-slide` = "prev",
      tags$span(class = "carousel-control-prev-icon", `aria-hidden` = "true"),
      tags$span(class = "sr-only", "Previous")
    ),
    tags$a(
      class = "carousel-control-next",
      href = "#carouselExampleIndicators",
      role = "button",
      `data-slide` = "next",
      tags$span(class = "carousel-control-next-icon", `aria-hidden` = "true"),
      tags$span(class = "sr-only", "Next")
    )
  )
}
