#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(glue)
setwd(here::here())
source("fcts.R")

estimator_tabs <- tabsetPanel(
    id = "estimators",
    type = "hidden",
    tabPanel(
        "none"
    ),
    tabPanel(
        "mean",
        htmlOutput("description_mean", container = p),
        uiOutput("simu_panel_mean")
    ),
    tabPanel(
        "variance",
        tabsetPanel(
            id = "variance",
            type = "hidden",
            tabPanel(
                "sure_about_variance",
                p("So, you want to look at the variance of the sample variance. 
              That is a bold choice. 
              But did you already look at the sample mean? 
              If so, then proceed. 
              Otherwise, you may want to look at the that first.
              Do you really want to proceed with the sample variance?"),
                selectInput(
                    "proceed_variance",
                    label = NULL,
                    choices = c("You sure?", "Yes, I know what I am doing.")
                )
            ),
            tabPanel(
                "proceed_variance",
                htmlOutput("description_var", container = p),
                uiOutput("simu_panel_var")
            )
        )
    )
)



## Define UI 
ui <- fluidPage(
    useShinyjs(),
    theme = bslib::bs_theme(bootswatch = "superhero"),
    
    navbarPage(
        "Statistical Concepts",
        tabPanel(
            "Variance of the sample variance",
            h1("Variance of an estimator"),
            p(
                "In this somewhat short and basic web app we want to explore the notion 
                of an estimator's variance.
                This is just to help clarify things like 'the variance of the sample variance' 
                which might sound somewhat convoluted and complex.
                In fact, it is not. This is what we want to learn here.
                To do so, we will look at the sample mean and the sample variance
                which are estimators for the mean or the variance of the underlying distribution
                of a sample.
                In mathematical notation, we will look at "
            ),
            withMathJax("$$\\overline{X}_n = \\frac{1}{n} \\sum_{k = 1}^n X_k \\quad \\text{or} \\quad S_n^2 = \\frac{1}{n} \\sum_{k = 1}^n (X_k - \\overline{X}_n)^2$$"),
            selectInput(
                "estimator_choice", 
                "Before we begin, choose an Estimator.",
                choices = c("Make a choice", "Sample mean", "Sample variance")
            ),
            estimator_tabs
        ),
        tabPanel(
            "Kernel density estimator",
            h1("Kernel density Estimator"),
            withMathJax(
                "A common estimator for the density function \\(f \\) using a sample \\((X_1, \\ldots, X_n)\\) is given by
                $$
                    \\hat{f}_n(x) = \\frac{1}{nh} \\sum_{k = 1}^n K\\bigg( \\frac{x - X_k}{h} \\bigg),
                $$
                where \\( K \\) is a so-called kernel function and \\(h > 0\\) is the so-called bandwidth operator.
                While this formula may feel intimidating, the idea is actually pretty simple.
                First, we pick a kernel function.
                Often, this is just the density function of a distribution.
                Imagine that we pick the density function from a standard normal distribution.
                Then, second, we 'place' one standard normal density function at each \\( X_k \\) and scale that by \\( h \\).
                Finally, for each \\(x \\in \\mathbb{R} \\) we average the function values of all kernel functions.
                "
            ),
            p(HTML("
                   For a visualization of this check out the animation I built a while back <a href='https://albert-rapp.de/post/visualize-kernel-density-estimation/kernelAnimation.gif'> here </a>.
                   For an interactive demonstration of kernel density estimation let us simulate samples from an exponential distribution.
                   In this first panel, notice how the width of the kernel function changes with the bandwidth.
                   This is essentially the role of the bandwith. 
                   Also, see how this changes the density estimates.
                   To avoid that the picture becomes too crowded with kernel functions, here let us stick to small sample sizes.
                  ")),
            sidebarLayout(
                sidebarPanel(
                    sliderInput(
                        "kernel_bandwidth",
                        "Bandwidth",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.1,
                        ticks = F
                    ),
                    selectInput(
                        "kernel_fct",
                        "Kernel function",
                        choices = c("Gaussian", "Epanechnikov", "Bisquare", "Uniform")
                    ),
                    p("To simulate a new underlying sample, adjust the following sliders and hit the button."),
                    sliderInput(
                        "kernel_sample_length",
                        "Sample length",
                        min = 1,
                        max = 10,
                        value = 5,
                        step = 1,
                        ticks = F
                    ),
                    sliderInput(
                        "kernel_lambda",
                        "Parameter lambda",
                        min = 0.01,
                        max = 1.5,
                        value = 1 / 2,
                        ticks = F
                    ),
                    actionButton("kernel_sample_draw", label = "Draw new sample", width = "100%")
                ),
                mainPanel(
                    plotOutput("kernel_plot1")
                )
            ),
            p(
                "Now that you (hopefully) understand the mechanism behind kernel density estimation, 
                try to find a bandwidth such that the estimation resembles the true density.
                Maybe you can increase the sample size to improve the quality of the estimate."
            ),
            sidebarLayout(
                sidebarPanel(
                    sliderInput(
                        "kernel_bandwidth2",
                        "Bandwidth",
                        min = 0,
                        max = 4,
                        value = 1,
                        step = 0.05,
                        ticks = F
                    ),
                    selectInput(
                        "kernel_fct2",
                        "Kernel function",
                        choices = c("Gaussian", "Epanechnikov", "Bisquare", "Uniform")
                    ),
                    p("To simulate a new underlying sample, adjust the following sliders and hit the button."),
                    sliderInput(
                        "kernel_sample_length2",
                        "Sample length",
                        min = 1,
                        max = 200,
                        value = 15,
                        step = 1,
                        ticks = F
                    ),
                    sliderInput(
                        "kernel_lambda2",
                        "Parameter lambda",
                        min = 0.01,
                        max = 1.5,
                        value = 1 / 2,
                        ticks = F
                    ),
                    actionButton("kernel_sample_draw2", label = "Draw new sample", width = "100%")
                ),
                mainPanel(
                    plotOutput("kernel_plot2")
                )
            )
        )
    )
    
    
    
)

## Define server logic 
server <- function(input, output) {
    
    ## KERNEL DENSITY PART
    lambda_kernel <- reactive({input$kernel_lambda})
    n_kernel <- reactive({input$kernel_sample_length})
    kernel_fct <- reactive({
        get_kernel_function(input$kernel_fct)
    })
    
    bw_kernel <- reactive({input$kernel_bandwidth})
    sample1 <- reactiveVal(rexp(5, rate = 1 / 2))
    observeEvent(
        input$kernel_sample_draw,
        sample1({rexp(n_kernel(), rate = lambda_kernel())})
    )
    
    output$kernel_plot1 <- renderPlot({
        kernels_from_sample <- tibble(
            Z = sample1()
        ) %>% 
            mutate(
                group = seq_along(Z),
                x = list(seq(-min(Z) - 2,  max(Z) + 2, 0.01))
            ) %>%
            unnest(x) %>%
            mutate(kernel = map2_dbl(
                Z, x, function(x, y) {
                    fct <- kernel_fct()
                    fct((y - x) / bw_kernel()) / bw_kernel() 
                } 
            )
            )
        
        estimates <- kernels_from_sample %>% 
            group_by(x) %>% 
            summarise(est = mean(kernel))
        
        
        kernels_from_sample %>% 
            ggplot(aes(x = x, y = kernel)) +
            geom_line(aes(group = group, col = "Kernel function"), size = 1) +
            geom_segment(
                aes(x = Z, xend = Z, y = 0, yend = kernel,
                    col = "Sample"),
                linetype = 2,
                size = 0.25
            ) +
            geom_line(
                data = estimates, 
                aes(x = x, y = est, col = "Estimation"),
                size = 1
            ) +
            geom_point(
                aes(x = Z, y = 0,
                    col = "Sample"),
                size = 3
            ) +
            scale_y_continuous(minor_breaks = NULL) +
            scale_x_continuous(minor_breaks = NULL) +
            scale_color_manual(values = c(oki_vermillion, "grey80", oki_blue)) +
            theme_light() +
            theme(text = element_text(size = 14)) +
            labs(x = "x", y = element_blank(), color = element_blank())
    })
    
    
    
    lambda_kernel2 <- reactiveVal(1 / 2)
    n_kernel2 <- reactive({input$kernel_sample_length2})
    kernel_fct2 <- reactive({
        get_kernel_function(input$kernel_fct2)
    })
    bw_kernel2 <- reactive({input$kernel_bandwidth2})
    sample2 <- reactiveVal(rexp(15, rate = 1 / 2))
    observeEvent(
        input$kernel_sample_draw2,{
            lambda_kernel2(input$kernel_lambda2)
            sample2({rexp(n_kernel2(), rate = lambda_kernel2())})
        }
    )
   
    output$kernel_plot2 <- renderPlot({
        kernel_sample <- tibble(
            Z = sample2()
        )
        min_range <- min(kernel_sample$Z)
        max_range <- max(kernel_sample$Z)
        
        dens_estimate <- tibble(
            x = seq(min_range, max_range, 0.01),
        ) %>% 
            mutate(
                est = map_dbl(
                    x, ~kernel_dens_estimate(., kernel_fct2(), bw_kernel2(), kernel_sample$Z)
                )
            )
        
        kernel_sample <- kernel_sample %>% 
            mutate(est = map_dbl(
                Z, ~kernel_dens_estimate(., kernel_fct2(), bw_kernel2(), kernel_sample$Z)
            ))
        
        
        
        dens_estimate %>% 
            ggplot(aes(x)) +
            stat_function(
                fun = ~dexp(., rate = lambda_kernel2()), 
                aes(col = "True density"),
                size = 1
            )  +
            geom_line(mapping = aes(y = est, col = "Estimation"), size = 1) +
            geom_point(
                data = kernel_sample,
                aes(y = 0, x = Z, col = "Sample"),
                size = 3
            ) +
            geom_segment(
                data = kernel_sample,
                aes(y = 0, yend = est, x = Z, xend = Z, col = "Sample"),
                linetype = 2,
                size = 0.25
            ) +
            theme_light() +
            theme(
                text = element_text(size = 14)
            ) +
            scale_color_manual(values = c(oki_vermillion, oki_blue, "grey80")) 
    })
    
    
    
    ## VARIANCE OF ESTIMATOR PART
    
    # Change between mean and variance window.
    isNone <- reactive({str_detect(input$estimator_choice, "Make")})
    isMean <- reactive({str_detect(input$estimator_choice, "mean")})
    isVariance <- reactive({!(isMean() | isNone())})
    
    observeEvent(input$estimator_choice, {
        updateTabsetPanel(
            inputId = "estimators", 
            selected = case_when(
                isMean() ~ "mean", 
                isVariance() ~ "variance",
                isNone() ~ "none"
            )
        )
    })
    
    # Proceed with variance?
    observeEvent(input$proceed_variance, {
        updateTabsetPanel(
            inputId = "variance",
            selected = case_when(
                str_detect(input$proceed_variance, "Yes") ~ "proceed_variance",
                TRUE ~ "sure_about_variance"
            )
        )
    })
    
    
    # Description
    description_mean <- reactiveVal(value = generate_descriptions("mean", 0))
    description_var <- reactiveVal(value = generate_descriptions("var", 0))
    output$description_mean <- renderText({
        withMathJax()
        description_mean()
    })
    output$description_var <- renderText({description_var()})
    
    
    # Handle simu panel
    max_samples_mean <- reactiveVal(value = 1) # updated after first draw
    max_samples_var <- reactiveVal(value = 1)
    output$simu_panel_mean <- renderUI(
        isolate(create_sidebar_layout("mean", max_samples_mean()))
    )
    output$simu_panel_var <- renderUI(
        isolate(create_sidebar_layout("var", max_samples_var()))
    )
    
    # Handle graphics
    observeEvent(input$draw_sample_mean, {
        updateSliderInput(
            inputId = "n_samples_mean", 
            max = 100,
            step = 1
        )
        output$plot_mean <- renderPlot({
            create_plot(
                sample_length = isolate(input$sample_length_mean), 
                mu = isolate(input$true_mean_mean), 
                sd = isolate(sqrt(input$true_var_mean)),
                n_samples = isolate(input$n_samples_mean), 
                estimator = mean
            )
        })
    }) 
    observeEvent(input$draw_sample_var, {
        updateSliderInput(
            inputId = "n_samples_var", 
            max = 100,
            step = 1
        )
        output$plot_var <- renderPlot({
            create_plot(
                sample_length = isolate(input$sample_length_var), 
                mu = isolate(input$true_mean_var), 
                sd = isolate(sqrt(input$true_var_var)),
                n_samples = isolate(input$n_samples_var), 
                estimator = var
            )
        })
    })
    
    
    ## Buttons and counters and n_samples slider
    alert_time_text_change <- 250
    observeEvent(input$draw_sample_mean, {max_samples_mean(100)})
    observeEvent(input$draw_sample_var, {max_samples_var(100)})
    
    c_draws_mean <- reactiveVal(value = 0)
    observeEvent(input$draw_sample_mean, {
        tmp <- c_draws_mean()
        c_draws_mean(tmp + 1)
    })
    c_draws_var <- reactiveVal(value = 0)
    observeEvent(input$draw_sample_var, {
        tmp <- c_draws_var()
        c_draws_var(tmp + 1)
    })
    
    stage_mean <- reactiveVal(value = 0)
    observeEvent(input$prev_button_mean, {
        tmp <- stage_mean()
        stage_mean(max(tmp - 1, 0))
        description_mean(generate_descriptions("mean", stage_mean()))
        delay(alert_time_text_change, description_mean(generate_descriptions("mean", stage_mean(), highlight = F)))
    })
    observeEvent(input$next_button_mean, {
        if (c_draws_mean() >= 1) {
            if (!str_detect(description_mean(), "Yay")) {
                tmp <- stage_mean()
                stage_mean(tmp + 1)
                description_mean(generate_descriptions("mean", stage_mean()))
                delay(alert_time_text_change, description_mean(generate_descriptions("mean", stage_mean(), highlight = F)))
            }
        }
        
    })
    
    stage_var <- reactiveVal(value = 0)
    observeEvent(input$prev_button_var, {
        tmp <- stage_var()
        stage_var(max(tmp - 1, 0))
        description_var(generate_descriptions("var", stage_var()))
        delay(alert_time_text_change, description_var(generate_descriptions("var", stage_var(), highlight = F)))
    })
    observeEvent(input$next_button_var, {
        if (c_draws_var() >= 1) {
            if (!str_detect(description_var(), "Hooray")) {
                tmp <- stage_var()
                stage_var(tmp + 1)  
                description_var(generate_descriptions("var", stage_var()))
                delay(alert_time_text_change, description_var(generate_descriptions("var", stage_var(), highlight = F)))
            }
        }
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
