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
library(shinyWidgets)
library(patchwork)
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
            withMathJax("$$\\overline{X}_n = \\frac{1}{n} \\sum_{k = 1}^n X_k \\quad \\text{or} \\quad S_n^2 = \\frac{1}{n - 1} \\sum_{k = 1}^n (X_k - \\overline{X}_n)^2$$"),
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
        ),
        tabPanel(
            'Confidence Intervals',
            h1('Confidence Intervals'),
            withMathJax('In statistical inference, one frequently tries to estimate an unknown parameter
              from a given distribution based on a data sample. For instance, if we assume that
              a sample consists of observations from a normal distribution with unknown mean \\(\\mu\\) and
              known standard deviation \\(\\sigma\\), then we might be interested to estimate \\(\\mu\\).
              But estimating just a single value is often not enough.
              This is where so-called confidence intervals come into play. 
              These are constructed by using so-called quantiles from a known distribution.
              To find out what that means, let us take a look at the \\( \\alpha \\)-quantiles of 
              the standard normal distribution.
              The following plot shows its density and its entire quantile function. 
              Play around with the slider to evaluate the quantile function at a value \\(\\alpha\\) 
              to get a feeling for the interplay here.'),
            hr(),
            sidebarLayout(
                sidebarPanel(
                    noUiSliderInput(
                        'alpha',
                        label = withMathJax('\\(\\alpha\\) '),
                        min = 0, max = 1, step = 0.01,
                        value = 0.9, 
                        orientation = "vertical",
                        direction = 'rtl',
                        width = "100px", height = "300px", 
                        color = '#df691a'
                    ), 
                    width = 2
                ),
                mainPanel(
                    plotOutput('single_alpha'),
                    width = 10
                )
            ),
            hr(),
            withMathJax('The orange area under the blue curve has an area of exactly \\(\\alpha\\). 
            This means that the \\( \\alpha \\)-quantile \\( q_a \\) of a continous distribution 
            (like the normal distribution) is defined such that \\( \\mathbb{P}(Z \\leq q_a) = \\alpha \\),
            where \\(Z\\) is just a random variable that follows that distribution.
            Similarly, we can use the exact same approach using quantiles to find a shaded area of any 
            given size (simply use two quantiles). '),
            hr(),
            sidebarLayout(
                sidebarPanel(
                    noUiSliderInput(
                        'alphas',
                        label = withMathJax('\\(\\alpha\\) '),
                        min = 0, max = 1, step = 0.01,
                        value = c(0.1, 0.9), margin = 0.01,
                        orientation = "vertical",
                        direction = 'rtl',
                        width = "100px", height = "300px", 
                        color = '#df691a'
                    ), 
                    width = 2
                ),
                mainPanel(
                    plotOutput('two_alphas'),
                    width = 10
                )
            ),
            hr(),
            withMathJax(
                '
            In our setting (normally distributed samples with unknown mean \\(\\mu\\) and known standard
            deviation \\(\\sigma\\)) we know that our statistic 
            $$
            T = \\sqrt{n} \\frac{\\overline{X_n} - \\mu}{\\sigma}
            $$
            is standard normally distributed (even if we do not know \\(\\mu\\)). 
            Thus, we can use the previous technique to find constants \\(c_1\\) and \\(c_2\\) such that 
            $$\\mathbb{P}(c_1 \\leq T \\leq c_2) = \\gamma \\in (0, 1).$$ 
            Where \\( \\gamma \\) is an arbitrary value in \\( (0, 1) \\).
            The point here is that we can use the standard normal quantile function to find 
            constants such that the probability (i.e. the previous orange area) is as large as we want.
            Rewriting the equation we get 
            $$
            \\mathbb{P}\\bigg(\\overline{X_n} - \\frac{\\sigma}{n}c_1 \\leq \\mu \\leq \\overline{X_n} - \\frac{\\sigma}{n}c_2\\bigg) = \\gamma.
            $$  
            This will give us a random interval and the interpretation here is that
            \\(\\gamma\\) of these randomly generated intervals will contain the 
            "true" value \\(\\mu\\).
            To see that, pick two new thresholds for \\( \\alpha \\) to pick two quantiles and simulate \\( N \\)  samples 
            of length \\( n \\) from a normal distribution with mean \\(\\mu\\)  and standard 
            deviation \\(\\sigma\\) and the resulting plot will tell you how 
            many intervals contain \\(\\mu\\).
            '),
            hr(),
            sidebarLayout(
                sidebarPanel(
                    sliderInput(
                        'alpha_CIs',
                        withMathJax('\\(\\alpha\\)'),
                        min = 0,
                        max = 1,
                        value = c(0.1, 0.9),
                        step = 0.01,
                        width = '100%'
                    ),
                    numericInput(
                        'N_CIs',
                        withMathJax('Number of samples \\(N\\)'),
                        min = 1,
                        max = 10000,
                        value = 25,
                        step = 1
                    ),
                    numericInput(
                        'n_CIs',
                        withMathJax('Sample length \\(n\\)'),
                        min = 10,
                        max = 1000,
                        value = 15,
                        step = 1
                    ),
                    sliderInput(
                        'mu_CIs',
                        withMathJax('True mean \\(\\mu\\)'),
                        min = -3,
                        max = 3,
                        value = 0,
                        step = 0.1
                    ),
                    sliderInput(
                        'sigma_CIs',
                        withMathJax('True standard deviation \\(\\sigma\\)'),
                        min = 0.1,
                        max = 5,
                        value = 1,
                        step = 0.1
                    ),
                    checkboxInput(
                        'show_means',
                        'Display sample means?',
                        value = F
                    )
                ),
                mainPanel(
                    plotOutput('plot_CIs', height = '575px' )
                )
            ),
            hr(),
            p('Spoiler alert: There is a lot to learn from this simple plot and 
            I will spill it out for you. 
              So if you want to play around with the plot, you may want to do 
              that first, before you read on.'),
            checkboxInput(
                'readon_CI',
                'If you want to continue reading, check this box.'
            ),
            uiOutput('readon_CI_UI')
        )
    )
    
    
    
)

## Define server logic 
server <- function(input, output) {
    
    ## CONFIDENCE INTERVALS PART
    
    normal_tib <- reactive({
        tibble(
            x = seq(-5, 5, 0.01), 
            y = dnorm(x)
        ) 
    })
    
    quantile_tib <- reactive({
        tibble(
            x = seq(0, 1, 0.01),
            y = qnorm(x)
        )
    })
    
    output$single_alpha <- renderPlot({
        dens_plot <- normal_tib() %>% 
            ggplot(aes(x = x)) +
            geom_ribbon(
                data = normal_tib() %>% filter(x <= qnorm(input$alpha)),
                aes(ymin = 0, ymax = y),
                col = NA,
                fill = oki_vermillion
            ) + 
            geom_line(aes(y = y), size = 1.5, col = oki_blue) +
            ggtitle(label = paste('The size of the orange area is equal to', input$alpha))
        
        quant_plot <- quantile_tib() %>% 
            ggplot(aes(x, y)) +
            geom_line(size = 1.5, col = oki_blue) +
            geom_segment(
                data = quantile_tib() %>% filter(x == input$alpha), 
                aes(xend = x, yend = -3), 
                linetype = 2,
                size = 1,
                col = oki_vermillion
            ) +
            geom_segment(
                data = quantile_tib() %>% filter(x == input$alpha), 
                aes(xend = 0, yend = y), 
                linetype = 2,
                size = 1,
                col = oki_vermillion
            ) +
            geom_point(
                data = quantile_tib() %>% filter(x == input$alpha),
                size = 3,
                col = oki_vermillion
            ) +
            geom_hline(yintercept = 0, col = 'black') +
            coord_cartesian(expand = F) +
            ggtitle('The point describes the right cutoff point')
        
        dens_plot + quant_plot & theme_light() & theme(
            text = element_text(face = 'bold'),
            plot.title.position = 'plot'
        )
    })
    
    alpha1 <- reactive(input$alphas[1])
    alpha2 <- reactive(input$alphas[2])
    
    output$two_alphas <- renderPlot({
        dens_plot2 <- normal_tib() %>% 
            ggplot(aes(x = x)) +
            geom_ribbon(
                data = normal_tib() %>% filter(between(x, qnorm(alpha1()), qnorm(alpha2()))),
                aes(ymin = 0, ymax = y),
                col = NA,
                fill = oki_vermillion
            ) + 
            geom_line(aes(y = y), size = 1.5, col = oki_blue) +
            ggtitle(label = paste('The size of the orange area is equal to', alpha2() - alpha1()))
        
        
        quantile_tib <- tibble(
            x = seq(0, 1, 0.01),
            y = qnorm(x)
        )
        
        quant_plot2 <- quantile_tib() %>% 
            ggplot(aes(x, y)) +
            geom_line(size = 1.5, col = oki_blue) +
            geom_segment(
                data = quantile_tib() %>% filter(x == alpha1() | x == alpha2()), 
                aes(xend = x, yend = -3), 
                linetype = 2,
                size = 1,
                col = oki_vermillion
            ) +
            geom_segment(
                data = quantile_tib() %>% filter(x == alpha1() | x == alpha2()), 
                aes(xend = 0, yend = y), 
                linetype = 2,
                size = 1,
                col = oki_vermillion
            ) +
            geom_point(
                data = quantile_tib() %>% filter(x == alpha1() | x == alpha2()),
                size = 3,
                col = oki_vermillion
            ) +
            geom_hline(yintercept = 0, col = 'black') +
            coord_cartesian(expand = F) +
            ggtitle('The points describes the left and right cutoff points')
        
        dens_plot2 + quant_plot2 & theme_light() & theme(
            text = element_text(face = 'bold'),
            plot.title.position = 'plot'
        )
    })
    
    alpha1_CIs <- reactive({input$alpha_CIs[1]})
    alpha2_CIs <- reactive({input$alpha_CIs[2]})
    N <-  reactive({input$N_CIs})
    n <-  reactive({input$n_CIs})
    sigma <- reactive({input$sigma_CIs})
    mu <- reactive({input$mu_CIs})
    display_mean <- reactive({input$show_means})
    
    CIs <- reactive({
        tibble(
            i = 1:N(),
            CI = map(i, ~conf(n(), mu(), sigma(), alpha1_CIs(), alpha2_CIs()))
        ) %>%  
            tidyr::unnest_wider(CI) %>% 
            mutate(mu_contained = map2_lgl(lower, upper, ~between(mu(), .x, .y)))
    })
    
    mu_contained <- reactive({mean(CIs()$mu_contained)})
    
    
    output$plot_CIs <- renderPlot({
        p <- CIs() %>%  
            ggplot(aes(x = lower, xend = upper, y = i, yend = i)) +
            geom_segment() +
            geom_vline(xintercept = mu(), col = oki_vermillion, size = 2) +
            coord_cartesian(xlim = c(mu() - 1, mu() + 1)) +
            ggtitle(paste0('Intervals containing true mean: ', scales::percent(mu_contained(), 0.1),
                           ' (Difference of alphas: ', scales::percent(alpha2_CIs() - alpha1_CIs(), 0.1), ')')) +
            theme_light() +
            theme(
                text = element_text(face = 'bold'),
                plot.title.position = 'plot'
            ) +
            scale_y_continuous(labels = round) +
            labs(x =  element_blank(), y = element_blank()) 
        
        if (display_mean()) {
            p <- p + 
                geom_point(aes(x = mean), size = 3, col = oki_blue)
        }
        p
    })
    
    output$readon_CI_UI <- renderUI({
        if (input$readon_CI) {
            withMathJax(includeMarkdown('CIs.md'))
        }
    })
    
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
