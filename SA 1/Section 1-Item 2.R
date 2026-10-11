#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# ---------- Helper functions ----------

parse_vector <- function(txt) {
  if (trimws(txt) == "") return(numeric(0))
  as.numeric(strsplit(trimws(txt), ",", fixed = TRUE)[[1]])
}

parse_matrix <- function(txt) {
  rows <- strsplit(
    gsub("\r", "", trimws(txt)),
    "\n",
    fixed = TRUE
  )[[1]]
  
  values <- lapply(rows, function(row) {
    as.numeric(strsplit(trimws(row), ",", fixed = TRUE)[[1]])
  })
  
  if (length(values) == 0 ||
      any(lengths(values) != length(values[[1]]))) {
    stop("Every row must contain the same number of probabilities.")
  }
  
  do.call(rbind, values)
}

valid_distribution <- function(p) {
  all(is.finite(p)) &&
    all(p >= 0 & p <= 1) &&
    abs(sum(p) - 1) < 1e-8
}

get_stats <- function(x, p) {
  mu <- sum(x * p)
  variance <- sum((x - mu)^2 * p)
  
  data.frame(
    Mean = mu,
    Variance = variance
  )
}

# ---------- User interface ----------

ui <- fluidPage(
  titlePanel("Discrete Probability Calculator"),
  
  p("Calculate statistics and explore discrete probability distributions."),
  
  tabsetPanel(
    tabPanel(
      "Univariate",
      
      h4("Enter the values and probabilities"),
      helpText("Separate values with commas. Example: 0, 1, 2"),
      
      textInput("u_x", "Values of X:", "0, 1, 2"),
      textInput("u_p", "Probabilities:", "0.2, 0.5, 0.3"),
      
      actionButton("calc_u", "Calculate"),
      
      h4("Results"),
      tableOutput("u_stats"),
      tableOutput("u_table"),
      
      h4("Probability Mass Function (PMF)"),
      plotOutput("u_pmf"),
      
      h4("Cumulative Distribution Function (CDF)"),
      plotOutput("u_cdf")
    ),
    
    tabPanel(
      "Bivariate",
      
      h4("Enter the joint probability distribution"),
      helpText("Rows correspond to X values; columns correspond to Y values."),
      
      textInput("b_x", "Values of X:", "0, 1"),
      textInput("b_y", "Values of Y:", "0, 1"),
      
      textAreaInput(
        "b_joint",
        "Joint probabilities (one row per X value):",
        value = "0.2, 0.3\n0.1, 0.4",
        rows = 4
      ),
      
      actionButton("calc_b", "Calculate"),
      
      h4("Marginal distributions"),
      h5("Marginal distribution of X"),
      tableOutput("b_marg_x"),
      h5("Marginal distribution of Y"),
      tableOutput("b_marg_y"),
      
      h4("Means and variances"),
      tableOutput("b_stats"),
      
      h4("Conditional distributions"),
      h5("Distribution of Y given X"),
      tableOutput("b_cond_yx"),
      h5("Distribution of X given Y"),
      tableOutput("b_cond_xy"),
      
      h4("Conditional means and variances"),
      tableOutput("b_cond_stats"),
      
      h4("Marginal PMF and CDF plots"),
      fluidRow(
        column(6, plotOutput("b_pmf_x")),
        column(6, plotOutput("b_cdf_x"))
      ),
      fluidRow(
        column(6, plotOutput("b_pmf_y")),
        column(6, plotOutput("b_cdf_y"))
      ),
      
      h4("Conditional PMF and CDF plots"),
      selectInput("cond_x", "Choose X for P(Y | X):", choices = NULL),
      plotOutput("cond_yx_pmf"),
      plotOutput("cond_yx_cdf"),
      
      selectInput("cond_y", "Choose Y for P(X | Y):", choices = NULL),
      plotOutput("cond_xy_pmf"),
      plotOutput("cond_xy_cdf")
    )
  )
)

# ---------- Server: calculations ----------

server <- function(input, output, session) {
  
  # Univariate distribution
  uni <- eventReactive(input$calc_u, {
    x <- parse_vector(input$u_x)
    p <- parse_vector(input$u_p)
    
    validate(
      need(length(x) > 0, "Enter at least one X value."),
      need(length(x) == length(p),
           "X values and probabilities must have equal lengths."),
      need(all(is.finite(x)), "X values must be finite numbers."),
      need(!anyDuplicated(x), "X values must be unique."),
      need(valid_distribution(p),
           "Probabilities must be in [0, 1] and sum to 1.")
    )
    
    ord <- order(x)
    x <- x[ord]
    p <- p[ord]
    
    list(x = x, p = p, stats = get_stats(x, p))
  })
  
  output$u_stats <- renderTable({
    uni()$stats
  }, digits = 5)
  
  output$u_table <- renderTable({
    d <- uni()
    data.frame(
      X = d$x,
      Probability = d$p,
      CDF = cumsum(d$p)
    )
  }, digits = 5)
  
  output$u_pmf <- renderPlot({
    d <- uni()
    plot(
      d$x, d$p,
      type = "h", lwd = 3,
      main = "PMF of X",
      xlab = "X", ylab = "P(X = x)"
    )
    points(d$x, d$p, pch = 16)
  })
  
  output$u_cdf <- renderPlot({
    d <- uni()
    plot(
      d$x, cumsum(d$p),
      type = "s", lwd = 2,
      main = "CDF of X",
      xlab = "X", ylab = "P(X <= x)",
      ylim = c(0, 1)
    )
    points(d$x, cumsum(d$p), pch = 16)
  })
  
  # Bivariate joint distribution
  biv <- eventReactive(input$calc_b, {
    x <- parse_vector(input$b_x)
    y <- parse_vector(input$b_y)
    
    validate(
      need(length(x) > 0 && length(y) > 0,
           "Enter values for both X and Y."),
      need(all(is.finite(x)) && all(is.finite(y)),
           "X and Y values must be finite numbers."),
      need(!anyDuplicated(x) && !anyDuplicated(y),
           "X and Y values must be unique.")
    )
    
    joint <- tryCatch(
      parse_matrix(input$b_joint),
      error = function(e) matrix(numeric(0), nrow = 0, ncol = 0)
    )
    
    validate(
      need(!is.null(joint),
           "Enter a rectangular matrix of numeric probabilities."),
      need(nrow(joint) == length(x) &&
             ncol(joint) == length(y),
           "Matrix rows must match X values and columns must match Y values."),
      need(valid_distribution(as.vector(joint)),
           "Joint probabilities must be in [0, 1] and sum to 1.")
    )
    
    # Sort values so the CDF plots follow numerical order
    ix <- order(x)
    iy <- order(y)
    x <- x[ix]
    y <- y[iy]
    joint <- joint[ix, iy, drop = FALSE]
    
    # Marginal distributions
    px <- rowSums(joint)
    py <- colSums(joint)
    
    # Conditional probabilities
    # Rows: P(Y | X); columns: P(X | Y)
    cond_yx <- matrix(NA_real_, nrow = length(x), ncol = length(y))
    cond_xy <- matrix(NA_real_, nrow = length(x), ncol = length(y))
    
    for (i in seq_along(x)) {
      if (px[i] > 0) cond_yx[i, ] <- joint[i, ] / px[i]
    }
    
    for (j in seq_along(y)) {
      if (py[j] > 0) cond_xy[, j] <- joint[, j] / py[j]
    }
    
    list(
      x = x, y = y, joint = joint,
      px = px, py = py,
      cond_yx = cond_yx, cond_xy = cond_xy,
      stats_x = get_stats(x, px),
      stats_y = get_stats(y, py)
    )
  })
  
  # Update conditional plot selectors
  observeEvent(biv(), {
    d <- biv()
    updateSelectInput(
      session, "cond_x",
      choices = setNames(seq_along(d$x), d$x),
      selected = 1
    )
    updateSelectInput(
      session, "cond_y",
      choices = setNames(seq_along(d$y), d$y),
      selected = 1
    )
  })
  
  output$b_marg_x <- renderTable({
    d <- biv()
    data.frame(
      X = d$x,
      Probability = d$px,
      CDF = cumsum(d$px)
    )
  }, digits = 5)
  
  output$b_marg_y <- renderTable({
    d <- biv()
    data.frame(
      Y = d$y,
      Probability = d$py,
      CDF = cumsum(d$py)
    )
  }, digits = 5)
  
  output$b_stats <- renderTable({
    d <- biv()
    data.frame(
      Distribution = c("Marginal X", "Marginal Y"),
      Mean = c(d$stats_x$Mean, d$stats_y$Mean),
      Variance = c(d$stats_x$Variance, d$stats_y$Variance)
    )
  }, digits = 5)
  
  output$b_cond_yx <- renderTable({
    d <- biv()
    
    cdf_matrix <- t(apply(d$cond_yx, 1, cumsum))
    
    out <- expand.grid(X = d$x, Y = d$y)
    out$Probability <- as.vector(d$cond_yx)
    out$CDF <- as.vector(cdf_matrix)
    
    out
  }, digits = 5)
  
  output$b_cond_xy <- renderTable({
    d <- biv()
    out <- expand.grid(X = d$x, Y = d$y)
    out$Probability <- as.vector(d$cond_xy)
    out$CDF <- unlist(lapply(seq_along(d$y), function(j) {
      if (d$py[j] == 0) rep(NA_real_, length(d$x))
      else cumsum(d$cond_xy[, j])
    }))
    out
  }, digits = 5)
  
  output$b_cond_stats <- renderTable({
    d <- biv()
    
    yx <- lapply(seq_along(d$x), function(i) {
      if (d$px[i] == 0) return(c(NA_real_, NA_real_))
      unlist(get_stats(d$y, d$cond_yx[i, ]))
    })
    
    xy <- lapply(seq_along(d$y), function(j) {
      if (d$py[j] == 0) return(c(NA_real_, NA_real_))
      unlist(get_stats(d$x, d$cond_xy[, j]))
    })
    
    data.frame(
      Conditional = c(
        paste0("Y | X = ", d$x),
        paste0("X | Y = ", d$y)
      ),
      Mean = c(
        vapply(yx, `[`, numeric(1), 1),
        vapply(xy, `[`, numeric(1), 1)
      ),
      Variance = c(
        vapply(yx, `[`, numeric(1), 2),
        vapply(xy, `[`, numeric(1), 2)
      )
    )
  }, digits = 5)
  
  # Marginal PMF and CDF plots
  output$b_pmf_x <- renderPlot({
    d <- biv()
    plot(d$x, d$px, type = "h", lwd = 3,
         main = "Marginal PMF of X",
         xlab = "X", ylab = "P(X = x)")
    points(d$x, d$px, pch = 16)
  })
  
  output$b_cdf_x <- renderPlot({
    d <- biv()
    plot(d$x, cumsum(d$px), type = "s", lwd = 2,
         main = "Marginal CDF of X",
         xlab = "X", ylab = "P(X <= x)", ylim = c(0, 1))
  })
  
  output$b_pmf_y <- renderPlot({
    d <- biv()
    plot(d$y, d$py, type = "h", lwd = 3,
         main = "Marginal PMF of Y",
         xlab = "Y", ylab = "P(Y = y)")
    points(d$y, d$py, pch = 16)
  })
  
  output$b_cdf_y <- renderPlot({
    d <- biv()
    plot(d$y, cumsum(d$py), type = "s", lwd = 2,
         main = "Marginal CDF of Y",
         xlab = "Y", ylab = "P(Y <= y)", ylim = c(0, 1))
  })
  
  # Conditional plots: Y given selected X
  output$cond_yx_pmf <- renderPlot({
    d <- biv()
    i <- as.integer(input$cond_x)
    req(length(i) == 1, !is.na(i))
    p <- d$cond_yx[i, ]
    
    validate(need(d$px[i] > 0,
                  "Conditional distribution undefined: P(X = x) is zero."))
    
    plot(d$y, p, type = "h", lwd = 3,
         main = paste("PMF of Y | X =", d$x[i]),
         xlab = "Y", ylab = "P(Y = y | X = x)")
    points(d$y, p, pch = 16)
  })
  
  output$cond_yx_cdf <- renderPlot({
    d <- biv()
    i <- as.integer(input$cond_x)
    req(length(i) == 1, !is.na(i))
    validate(need(d$px[i] > 0,
                  "Conditional distribution undefined: P(X = x) is zero."))
    
    plot(d$y, cumsum(d$cond_yx[i, ]), type = "s", lwd = 2,
         main = paste("CDF of Y | X =", d$x[i]),
         xlab = "Y", ylab = "Conditional CDF", ylim = c(0, 1))
  })
  
  # Conditional plots: X given selected Y
  output$cond_xy_pmf <- renderPlot({
    d <- biv()
    j <- as.integer(input$cond_y)
    req(length(j) == 1, !is.na(j))
    
    validate(need(d$py[j] > 0,
                  "Conditional distribution undefined: P(Y = y) is zero."))
    
    plot(d$x, d$cond_xy[, j], type = "h", lwd = 3,
         main = paste("PMF of X | Y =", d$y[j]),
         xlab = "X", ylab = "P(X = x | Y = y)")
    points(d$x, d$cond_xy[, j], pch = 16)
  })
  
  output$cond_xy_cdf <- renderPlot({
    d <- biv()
    j <- as.integer(input$cond_y)
    req(length(j) == 1, !is.na(j))
    
    validate(need(d$py[j] > 0,
                  "Conditional distribution undefined: P(Y = y) is zero."))
    
    plot(d$x, cumsum(d$cond_xy[, j]), type = "s", lwd = 2,
         main = paste("CDF of X | Y =", d$y[j]),
         xlab = "X", ylab = "Conditional CDF", ylim = c(0, 1))
  })
}

shinyApp(ui = ui, server = server)
