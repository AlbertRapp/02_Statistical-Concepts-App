create_sidebar_panel <- function(id, max_samples) {
  sidebarPanel(
    withMathJax(),
    # selectInput(
    #   glue("distribution_choice_{id}"), 
    #   label = "Distribution", 
    #   choices = "Normal distribution"
    # ),
    
    sliderInput(
      glue("sample_length_{id}"),
      label = 'Sample length',
      min = 3,
      max = 100,
      value = 10,
      ticks = F
    ),
    sliderInput(
      glue("true_mean_{id}"),
      label = 'True mean \\(\\mu\\)',
      min = 1,
      max = if_else(id == "mean", 4, 20),
      value = 2,
      step = 0.5,
      ticks = F
    ),
    sliderInput(
      glue("true_var_{id}"),
      label = 'True variance \\(\\sigma^2\\)',
      min = 1,
      max = 20,
      value = 5,
      step = 0.5,
      ticks = F
    ),
    sliderInput(
      glue("n_samples_{id}"),
      "Number of Samples",
      min = 1,
      max = max_samples,
      value = 1,
      ticks = F
    ),
    fluidRow(
      column(
        12,
        splitLayout(
          cellWidths = c("60%", "20%", "20%"),
          actionButton(glue("draw_sample_{id}"), "Draw!", width = '100%'),
          actionButton(glue("prev_button_{id}"), "prev", width = '100%'),
          actionButton(glue("next_button_{id}"), "next", width = '100%')
          )
        )
    )
  )
}

create_sidebar_layout <- function(id, max_samples) {
  sidebarLayout(
    create_sidebar_panel(id, max_samples),
    mainPanel(
      plotOutput(glue("plot_{id}"))
    )
  )
}


create_plot <- function(sample_length, mu, sd, n_samples, estimator) {
  oki_blue <- "#0072B2"
  oki_vermillion <- "#D55E00"
  oki_green <- "#009E73"
  est_name <- if_else(
    as.character(substitute(estimator)) == "mean", "mean", "variance"
  )
  
  if (n_samples == 1) {
    sample <- rnorm(sample_length, mu, sd)
    single_est <- estimator(sample)
    ggplot() +
      geom_segment(aes(
        x = seq_along(sample), 
        xend = seq_along(sample), 
        y = 0, 
        yend = sample),
        size = 1,
        col = oki_vermillion
      ) +
      geom_point(
        aes(x = seq_along(sample), y = sample), col = oki_blue, size = 3
      ) +
      labs(
        x = "Observation number", 
        y = "Value",
        title = glue("The sample {est_name} is {round(single_est, 4)} for this sample")
      ) +
      theme_light() +
      theme(text = element_text(size = 14))
  } else {
    evaluated_samples <- tibble(
      sample_number = 1:n_samples
    ) %>% 
      mutate(
        sample = map(sample_number, ~rnorm(n = sample_length, mean = mu, sd = sd)),
        estimate = map_dbl(sample, estimator)
      )  
    
    evaluated_samples %>% 
      ggplot(aes(y = sample_number, x = estimate)) +
      geom_segment(
        aes(
          x = ifelse(est_name == "mean", mu, sd^2), 
          xend = estimate, 
          y = sample_number, 
          yend = sample_number
        ),
        linetype = 2,
        col = oki_vermillion
      ) +
      geom_vline(xintercept = ifelse(est_name == "mean", mu, sd^2), col = oki_vermillion, size = 2) +
      geom_point(size = 3, col = oki_blue) +
      coord_cartesian(xlim = c(0, x = ifelse(est_name == "mean", 5, 20))) +
      scale_x_continuous(minor_breaks = NULL) +
      scale_y_continuous(minor_breaks = NULL) +
      theme_light() +
      labs(x = "Estimator evaluated for sample", y = "Sample number") +
      theme_light() +
      theme(
        panel.grid.major.y = element_blank(), 
        text = element_text(size = 14)
      )
  }
}

generate_descriptions <- function(estimator, stage, highlight = T) {
  
  stage <- as.character(stage)
  text <- if (estimator == "mean") {
    switch(
      stage,
      
      "0" = " Aha! So you choose the sample mean? Splendid choice.
              Start our journey by drawing a sample.
              At first, we want to draw only a single sample, so you cannot 
              change how many samples are drawn.
              But feel free to tinker with the other variables and then hit 
              the 'Draw!' button.
              Afterwards, click 'next' to see further comments.",
      "1" = "That was exciting, wasn't it? 
              We have simulated a sample consisting of observations drawn from 
              a normal distribution and we can see what values each observation 
              takes.
              Even better, we can see what our estimator delivers for this 
              particular sample.
              Well, maybe it is not that exciting after all.
              Possibly, increasing the number of samples is more exciting.
              Try that and then hit 'next'.",
      "2" = "Woaaaah! That is some fancy stuff right there.
              We have simulated multiple samples and evaluated our estimator, 
              i.e. the sample mean, on each sample.
              This way, we can see how each estimate deviates from the true 
              mean (indicated by the bold orange line).
              Notice how I have used the word deviation?
              This means that, of course, our estimate of the true mean is
              not 100% correct and we get fluctuations.
              <br>
              <br>
              This is what variance is right?
              Squared deviation from the expected value.
              Try tinkering with the simulation parameters and see how the 
              picture changes.
              If you've had enough, hit 'next'.",
      "3" = "Elementary probability theory tells us that the variance of the
              sample mean is the quotient of the true variance and the length 
              of each sample.
              Therefore, we can decrease how much our estimates deviate from 
              the true mean by either increasing the sample size or decreasing 
              the true variance.
              If you have not seen that while tinkering in the previous step,
              then try that again.
              Otherwise, hit 'next'.",
      "4" = "Yay, you made it through this short course. 
              I hope you have a better feeling for the variance of the sample 
              mean now.
              Feel free to check out the sample variance as well.
              This is where students may get confused because we talk about the 
              variance of an estimator of the variance."
    )
  } else {
    switch(
      stage,
      "0" = "Ok, hotshot! 
              Let's get this ball rolling. 
              Draw <b>one</b> sample and then hit 'next'.",
      "1" = "If you have already looked at sample mean, then this output is 
              not surprising to you anymore.
              Here, we see a sample consisting of observations drawn from 
              a normal distribution and we can see what values each observation 
              takes.
              Also, we can see the value of the sample variance for that sample.
              Make things more exciting by increasing the number of samples
              and then hit 'next'.",
      "2" = "Alright, now we have more samples and an estimate of the true variance
              from each of those samples.
              Again, we can also see how much each estimate deviates from the true value.
              Now fix the true variance to some low value and try multiple values for
              the true mean.
              If you have tried a couple of means, hit 'next'.",
      "3" = "Did you notice that changing the mean does not change much?
              For the normal distribution, there is no relationship between the 
              true variance and the true mean. 
              Consequently, changing the true mean does not affect the estimation
              of the variance via the sample variance.
              <br>
              <br>
              Probably not that surprising right?
              Ok then. 
              Now, try to change other values to find out how they affect the 
              deviations of the estimates from the true variance.",
      "4" = "I hope you noticed that - same as with the sample mean - lowering the
              true variance and increasing the sample length makes the deviations of
              the estimates from the true value smaller, i.e. the variance of the
              sample variance becomes smaller.
              However, did you also notice that the variance of the sample variance 
              is way more sensitive to changes in the true variance compared to the sample
              mean?
              Don't believe me? Go back to the sample mean and increase the true
              variance step by step. Then, come back and do the same for the sample variance.
              Afterwards, hit 'next'.
              ",
      "5" = "Were you able to validate my claim about the higher sensitivity of
              the sample variance? If not, maybe try looking at a lot of samples
              with a small sample size, say 10. Then, increase the true variance
              step by step for both estimators.
              And if that didn't help, then you can always take a look at 
              <a href='https://en.wikipedia.org/wiki/Variance#Distribution_of_the_sample_variance'>
              Wikipedia</a>.
              There, you will find that the variance of the sample variance depends
              on the square of the true variance whereas the sample mean depends
              on the true variance (i.e. not squared).
              So, there is that.
              Hit 'next'.",
      
      "6" = "Hooray! You have made it through another mind-boggling journey with
              estimators. Good for you. Now, go have fun doing something else."
      
    )
  }
  
  if (highlight & stage > 0) {
    glue(
      '<mark style="background-color:#ec7063;font-size:14pt;"> {text} </mark>'
    )
  } else {
    text
  }
  
}
