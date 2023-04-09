#' @export
#' @name plot__fep
#' @title Visualize Feature Effects
#' @description Visualize feature effects for linear models of the form  y = X
#' * b
#' @param X dataset (n*m matrix)
#' @param b betas of linear model (named m*1 vector)
#' @param s sample of interest
#' @param debug draw colored borders to debug the plot
#' @param n_extreme how many samples from X to draw into the plot?
#' Samples are taken from both end of the y spectrum, i.e.
#' @param threshold threshold for classification setting
#' @param show_score show score in plot
#' @return `par(opar)``
plot__fep <- function(b,
                      X,
                      s,
                      debug = FALSE,
                      n_extreme = 10,
                      threshold = 0.991,
                      show_score = FALSE) {
  trace_func_entry("plot__fep")

  # Init symbols
  si <- s # sample of interest
  b_pos <- sort(b[b >= 0], decreasing = TRUE)
  b_neg <- sort(b[b < 0])
  b <- c(b_pos, b_neg)
  if (all(names(b) %in% colnames(X))) {
    Xt <- X
    X <- t(X[, names(b)])
    colnames(X) <- rownames(Xt)
  } else if (all(names(b) %in% rownames(X))) {
    Xt <- t(X)
    X <- X[names(b), ]
  } else {
    fnc <- features_not_in_colnames <- names(b)[!(names(b) %in% colnames(X))]
    fnr <- features_not_in_rownames <- names(b)[!(names(b) %in% rownames(X))]
    features_missing <- if (length(fnc) <= length(fnr)) fnc else fnr
    features_missing_str <- paste(features_missing, collapse = ", ")
    stop("The following model features are missing:\n", features_missing_str)
  }
  X_pos <- X[names(b_pos), ]
  X_neg <- X[names(b_neg), ]
  y_pos <- (t(X_pos) %*% b_pos)[, 1]
  y_neg <- (t(X_neg) %*% b_neg)[, 1]
  y <- (t(X) %*% b)[, 1]
  ticks <- data.frame("right" = c(cumsum(abs(b))))
  ticks$left <- c(0, ticks$right[-length(ticks$right)])
  if (show_score) {
    fig <- data.frame(
      signature = (c(0, 8, 0, 10) / 10),
      score <- (c(8, 10, 0, 10) / 10)
    )
  } else {
    fig <- data.frame(
      signature = (c(0, 10, 0, 10) / 10),
      score <- (c(10, 10, 0, 10) / 10)
    )
  }
  mar <- data.frame("signature" = (c(2, 3, 0, 0) + 0.1))
  mar$score <- (c(2, 3, 0, 0) + 0.1)
  x_max <- max(ticks$right)
  y_max <- ceiling(max(X))

  # Global Settings
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(opar) # oma=c(b, l, t, r), mfrow=c(1, 2), bty="n"
  graphics::par(xaxs = "i", yaxs = "i", xaxt = "n", oma = (c(0, 0, 2, 0)))

  # Signature area
  graphics::par(fig = fig$signature, mar = mar$signature)
  plot(
    x = NA, y = NA,
    xlim = c(0, x_max),
    ylim = c(0, y_max),
    xlab = NA, ylab = NA,
    yaxt = "s"
  )
  graphics::mtext("Expression", side = 2, line = 1.5)
  graphics::mtext("Genes ", side = 1, line = 0.5)

  # Grid and gene names
  bperc <- abs(b) / sum(abs(b))
  idx <- (bperc >= 0.03)
  graphics::abline(v = ticks$left)
  graphics::text(
    x = (ticks$left[idx] + ticks$right[idx]) / 2,
    y = 0,
    labels = paste0(names(b[idx]), " (", round(b[idx], 2), ")"),
    adj = c(0, 0.5), # x and y adjustment (0 = left/bottom, 1 = right/top)
    srt = 90 # rotation AFTER x-y-adjustment
  )

  # Lines for samples with lowest/highest score (in blue/red)
  y_sort <- sort(y)
  s <- names(y_sort) # sample names (sorted by score)
  n <- length(y)
  s_lowest <- s[1:n_extreme]
  s_highest <- s[(n - n_extreme + 1):n]
  if (n_extreme >= 1) {
    for (s in c(s_lowest, s_highest)) {
      col <- if (s %in% s_lowest) "blue" else "red"
      for (i in 1:length(b)) {
        f <- names(b)[i] # f == feature name
        graphics::lines(
          x = c(ticks$left[i], ticks$right[i]),
          y = c(X[f, s], X[f, s]),
          col = col
        )
      }
    }
  }

  # Sample of interest
  gep <- X[, si]
  names(gep) <- rownames(X)
  score <- y[si]
  col <- "orange"
  shortname <- substr(si, nchar(si) - 3, nchar(si))
  for (i in 1:length(b)) {
    gene <- names(b)[i]
    weight <- b[i]
    ge <- gep[gene]
    graphics::lines(
      x = c(ticks$left[i], ticks$right[i]),
      y = c(gep[gene], gep[gene]),
      col = col, lwd = 10
    )
    # Red rectangles --> increase score --> increase risk
    graphics::rect(
      xleft = ticks$left[i],
      ybottom = ifelse(
        weight >= 0,
        ge + ((threshold - score) / weight),
        0
      ),
      xright = ticks$right[i],
      ytop = ifelse(
        weight >= 0,
        y_max,
        ge + ((threshold - score) / weight)
      ),
      col = "#ff000020"
    )
    # Blue rectangles --> reduce score --> reduce risk
    graphics::rect(
      xleft = ticks$left[i],
      ybottom = ifelse(
        weight >= 0,
        0,
        ge + ((threshold - score) / weight)
      ),
      xright = ticks$right[i],
      ytop = ifelse(
        weight >= 0,
        ge + ((threshold - score) / weight),
        y_max
      ),
      col = "#0000ff20"
    )
  }
  graphics::legend("topleft", legend = si, fill = col, col = col)

  # Score area
  if (show_score) {
    graphics::par(fig = fig$score, new = TRUE, mar = mar$score)
    plot(
      x = NA, y = NA,
      yaxt = "s",
      xlim = c(0, 3), ylim = c(
        floor(min(y)),
        ceiling(max(c(y_pos, y_neg)))
      ),
      xlab = NA, ylab = NA
    )
    graphics::abline(v = c(1, 2))
    graphics::mtext("Score ", side = 1, line = 0.5)
    graphics::lines(x = c(2, 3), y = c(threshold, threshold), col = "black", lwd = 3)
    gep <- X[, si]
    names(gep) <- rownames(X)
    score <- y[si]
    col <- "orange"
    shortname <- substr(si, nchar(si) - 3, nchar(si))
    graphics::rect(0, 0, 1, y_pos[si], col = "#ff000040") # xleft ybot xright ytop
    graphics::rect(1, 0, 2, -y_neg[si], col = "#0000ff40")
    graphics::rect(2, 0, 3, score[si], col = "#DF902040")
  }

  # Outer Area
  graphics::mtext("Feature Effects", side = 3, line = 0.5, outer = TRUE)

  # Debug Boxes
  if (debug && show_score) {
    plot__fep__draw_debug_boxes(fig = fig$score, mar = mar$score)
  }

  graphics::par(opar)
}

plot__fep__draw_debug_boxes <- function(fig = NA,
                                        mar = NA,
                                        pcol = "red",
                                        fcol = "green",
                                        ocol = "blue") {
  # Signature
  if (any(!is.na(fig))) {
    graphics::par(fig = fig, mar = mar, new = TRUE)
    for (side in seq(4)) {
      if (mar[side] >= 1) {
        for (line in seq(0, mar[side])) {
          graphics::mtext(line, side = side, line = line, adj = 1, col = pcol)
          graphics::box(which = "plot", col = pcol)
        }
      }
    }
  }
  if (!is.na(pcol)) {
    graphics::box(which = "plot", col = pcol)
  }
  if (!is.na(fcol)) {
    graphics::box(which = "figure", col = fcol)
  }
  if (!is.na(ocol)) {
    graphics::box(which = "outer", col = ocol)
  }
}

# Genelist:
# > graphics::par(fig=(c(8, 10, 5, 10) / 10), new=TRUE, mar=c(0, 0, 2, 0))
# > plot(x=NA, y=NA, xlim=c(0, 10), ylim=c(0, length(b)), xlab=NA, ylab=NA)
# > graphics::mtext("0", side=3, line=0, adj=1, col="green")
# > graphics::mtext("1", side=3, line=1, adj=1, col="green")
# > graphics::mtext("Genelist", side=3, line=0.5)
# > box(which="figure", col="green")
# > box(which="plot", col="red")

# Plot Args:
# > main: main title of the plot (plot)

# graphics::Par Args:
# > adj: (0|1)=(left|right)-justify text, mtext and title (0.5=center)
# > ann: F = disable annotation of plots with axis- and overall titles
# > bg: background color (default: "white")
# > bty: Box type ("y"=default, "l", "7", "c", "u", "]", "n"=no box)
# > cex: Text and symbol magnification relative to default (0-Inf)
# > cexxis: Axis magnification relative to `cex'
# > cexab: Label magnification relative to `cex'
# > cexain: Main title
# > cexub: Sub title
# > col: default plotting color
# > colxis: axis annotation color
# > colab: x and y label color
# > colain: main title color
# > colub: sub-title color
# > cra: character size c(b, h) in pixels
# > (srt|crt): string/character roation in degrees (0-360)
# > c(in|si|xy): char size in (inches|inches|coord-units) as c(widht, height)
# > cxy: character size c(b, h) in user coordinate units
# > din: device dimensions c(b, h) in inches
# > family: The name of a font family for drawing text
# > fg: default foreground color
# > fig: display region coordinates for next plot.new c(x1, x2, y1, y2)
# > fin: The figure region dimensions c(b, h) in inches
# > font: 1=normal, 2=bold, 3=italic, 4=bold italic
# > font(xis|ab|ain|ub): (axis-annotation|label|main-title|sub-title)-font
# > lab: number of [xy]-ticks and label length as c(x, y, len) [c(5, 5, 7)]
# > las: style of axis labels (numeric in {0,1,2,3})
# > lend: line end style (integer or string)
# > lheight: line height multiplier
# > ljoin: line join style
# > lmitre: line mitre limit
# > lty: line type "(blank|solid|dashed|dotted|dotdash|longdash|twodash)"
# > lwd: line width (default: 1)
# > mai: margin size in inches as c(b, l, t, r)
# > mar: margin lines as c(b, l, t, r) [c(5.1, 4.1, 4.1, 2.1)]
# > mex: margin character size expansion factor
# > mf(col|row): plot by (cols|rows) in a grid of size c(nr, nc)
# > mfg: which figure to draw next c(i, j)
# > mgp: margin line (mex units) for axis-(title|labels|line) [c(3, 1, 0)]
# > new: TRUE = do not clean frame at next call to plot.new
# > oma: size of outer margins in lines of text as c(b, l, t, or)
# > omd: size of outer margins in normalized dev coords as c(x1, x2, y1, y2)
# > omi: size of outer margins in inches as c(b, l, t, r)
# > page: should next plot.new start a new page [FALSE]
# > pch: default plotting symbol (int or single char)
# > pin: plot dimensions c(b, h), in inches.
# > plt: plot region as fraction of figure region as c(x1, x2, y1, y2)
# > ps: point size of text (integer)
# > pty: plot region type ("s"=square, "m"=maximal plotting region)
# > tck: tick mark length as fraction of plotting region [NA: use tcl = -0.5]
# > tcl: tick mark length as fraction of line height [-0.5]
# > usr: plotting region coordinates as c(x1, x2, y1, y2)
# > [xy]axp: coords of extreme tick marks and number of intervals inbetween
# > [xy]axs: axis interval calculation style "(r|i|e|s|d)"
# > [xy]axt: axis type ("n"=no plotting) ["s"]
# > [xy]log: TRUE= use log scale
# > xpd: (FALSE|TRUE|NA) = clip plotting to (plot|figure|device)-region


#' @title Mean Standard Deviation Plot
#' @description The mean against standard deviation for each feature in a list
#' of datasets. The datasets must not contain non numeric columns.
#' @param dd list(char), dataset names
#' @param xx list(data.frame), covariate dataframes (must contain only numeric
#' columns)
#' @param xt char, how to transform the xaxis for plotting (options are
#' 'identity', 'log2', 'log10' and 'rank')
#' @param yt char, how to transform the xaxis for plotting (options are
#' 'identity', 'log2', 'log10' and 'rank')
#' @param ff vector(char), feature names to draw, if `util__is_none(ff)`, all
#' features are drawn
#' @param shape char, character shape to use, not implemented yet
#' @return TODO
plot__msd <- function(dd, xx, xt, yt, ff = NULL, shape = ".") {
  trace_func_entry("plot__msd")
  x <- y <- label <- dataset <- feature <- NULL
  if (length(dd) == 0) {
    df <- data.frame(x = 0, y = 0, label = "No dataset provided")
    ggplot(df, aes(x, y, label = label)) +
      geom_text() +
      theme_void()
  } else {
    if (!util__is_none(ff)) {
      xx2 <- lapply(xx, function(x) {
        cn <- colnames(x)
        ffexist <- intersect(ff, cn)
        ffna <- setdiff(ff, ffexist)
        x <- x[, ffexist]
        x
      })
    } else {
      xx2 <- xx
    }
    means <- nl_nv_2_df(
      lapply(xx2, function(x) apply(x, 2, mean)),
      col1 = "dataset",
      col2 = "feature",
      col3 = "mean"
    )
    sds <- nl_nv_2_df(
      lapply(xx2, function(x) apply(x, 2, stats::sd)),
      col1 = "dataset",
      col2 = "feature",
      col3 = "sd"
    )
    # head(means); tail(means);
    #           dataset   feature      mean
    #     1  lamis_train     RFC2  7.851505
    #     2  lamis_train    HSPA6  5.784336
    # 20657     ghsg.set   WDR83   9.108829
    # 20658     ghsg.set     WT1   7.029945
    # head(sds); tail(sds);
    #            dataset  feature         sd
    #     1  lamis_train     RFC2  0.6243982
    #     2  lamis_train    HSPA6  0.8377059
    # 20657     ghsg.set    WDR83  0.4851053
    # 20658     ghsg.set      WT1  1.3353375
    infomsg(sprintf("xt: %s, yt: %s", xt, yt))
    # browser(skipCalls = 1)
    means$mean <- switch(
      xt,
      identity = means$mean,
      log2 = log2(means$mean),
      log10 = log10(means$mean)
    )
    sds$sd <- switch(
      yt,
      identity = sds$sd,
      log2 = log2(sds$sd),
      log10 = log10(sds$sd)
    )
    data <- merge(means, sds, by = 1:2)
    # head(data); tail(data)
    #              dataset  feature       mean         sd
    #       1     ghsg.set      A2M  12.804626  0.7988832
    #       2     ghsg.set     ABAT   8.959297  0.6783653
    #   20657  lamis_train    ZZEF1   5.283571  0.2573278
    #   20658  lamis_train     ZZZ3   7.642813  0.3284888
    p <- ggplot(data = data, mapping = aes_string(x = "mean", y = "sd"))
    p <- p + geom_point(aes(color = dataset), data) # shape=shape
    if (!util__is_none(ff)) {
      n <- length(ff)
      p <- p + geom_text(
        aes(label = feature, col = dataset),
        hjust = -0.2, vjust = -0.2, size = 4, show.legend = FALSE
      )
    }
    # browser(skipCalls = 1)
    p <- p
    print(p)
  }
}


#' @export
#' @name plot__pred__hist
#' @title Plot Histogram of Predictions
#' @description Return ggplot2 object of histogram of Predictions
#' @param predictions as the name says (named list of named numeric vectors)
#' @param density_lines add density lines to the plot (logical)? (logical)
#' @param rug add rugs to the plot? (logical)
#' @param binwidth binwidth for histogram
#' @return ggplot2 object
plot__pred__hist <- function(predictions,
                             density_lines = TRUE,
                             rug = TRUE,
                             binwidth = NULL) {

  # TODO: remove tidyverse NSE shit to fix warnings below. Then remove stubs...
  # plot__pred__hist: no visible binding for global variable 'y'
  # plot__pred__hist: no visible binding for global variable 'dataset'
  y <- dataset <- NULL
  # END TODO

  # str(predictions)
  # $ lamis_train: c(GSM275076=-0.68, ..., GSM275377=+1.00) 233 samples
  # $ lamis_test1: c(GSM274895=-0.50, ..., GSM275196=-1.54) 181 samples
  df <- nl_nv_2_df(predictions)
  # head(df)
  #         dataset    sample     y
  #   1 lamis_train GSM275076 -0.68
  # ...         ...       ...   ...
  # 233 lamis_train GSM275377 +1.00
  # 234 lamis_test1 GSM274895 -0.50
  # ...         ...       ...   ...
  # 414 lamis_test1 GSM275196 -1.54
  binwidth <- binwidth %||% (ceiling10(diff(range(df$y))) / 100)
  p <- ggplot(data = df, mapping = aes(x = y))
  if (density_lines) {
    p <- p + geom_density(mapping = aes(col = dataset), size = 1)
  } else {
    p <- p + geom_histogram(
      mapping = aes(fill = dataset, col = dataset),
      binwidth = binwidth,
      alpha = 0.2,
      size = 1,
      position = "identity"
    )
  }
  if (rug) p <- p + geom_rug(aes(color = dataset))
  print(p)
}


plot__pca <- function(dd, xx, ff, ...) {
  trace_func_entry("plot__pca")
  xx2 <- lapply(xx, function(x) x[, ff])
  combined_matrix <- do.call("rbind", xx2)
  pca_result <- stats::prcomp(combined_matrix)
  pc1 <- pca_result$x[, 1]
  pc2 <- pca_result$x[, 2]
  ds_name = rep(dd, sapply(xx, nrow))
  pc_df <- data.frame(pc1 = pc1, pc2 = pc2, ds_name = ds_name)
  pc_df$sample_name <- unlist(lapply(xx, rownames))
  ggobj <- ggplot2::ggplot(pc_df, aes(x = pc1, y = pc2, color = ds_name)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "PC1", y = "PC2") +
    ggplot2::ggtitle("PCA Plot") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  print(ggobj)
}



plot__umap <- function(xx, dd, ff, n_neighbors = 15, min_dist = 0.1, n_components = 2) {
  xx2 <- lapply(xx, function(x) x[, ff])
  combined_matrix <- do.call("rbind", xx2)
  umap_result <- umap::umap(combined_matrix, n_neighbors = n_neighbors, min_dist = min_dist, n_components = n_components)
  umap1 <- umap_result$layout[, 1]
  umap2 <- umap_result$layout[, 2]
  ds_name <- rep(dd, sapply(xx, nrow))
  umap_df <- data.frame(umap1 = umap1, umap2 = umap2, ds_name = ds_name)
  umap_df$sample_name <- unlist(lapply(xx, rownames))
  ggobj <- ggplot2::ggplot(umap_df, aes(x = umap1, y = umap2, color = ds_name)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "UMAP1", y = "UMAP2") +
    ggplot2::ggtitle("UMAP Plot") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  print(ggobj)
}


plot__tsne <- function(xx, dd, ff, perplexity = 30, dims = 2, ...) {
  xx2 <- lapply(xx, function(x) x[, ff])
  combined_matrix <- do.call("rbind", xx2)
  tsne_result <- Rtsne::Rtsne(combined_matrix, perplexity = perplexity, dims = dims)
  tsne1 <- tsne_result$Y[, 1]
  tsne2 <- tsne_result$Y[, 2]
  ds_name = rep(dd, sapply(xx, nrow))
  tsne_df <- data.frame(tsne1 = tsne1, tsne2 = tsne2, ds_name = ds_name)
  tsne_df$sample_name <- unlist(lapply(xx, rownames))
  ggobj <- ggplot2::ggplot(tsne_df, aes(x = tsne1, y = tsne2, color = ds_name)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "t-SNE1", y = "t-SNE2") +
    ggplot2::ggtitle("t-SNE Plot") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  print(ggobj)
}


plot__pred <- function(b, X, s, ...) {
  features <- intersect(names(b), colnames(X))
  X2 <- X[, features, drop = FALSE]
  i <- which(rownames(X2) == s)
  n <- nrow(X2)
  n_pixel <- 400 + n * 5
  p_key <- 200 / n_pixel # always use 200 pixel for the color key
  p_heatmap <- 1 - p_key
  gplots::heatmap.2(
    x = as.matrix(X2),
    rowsep = c(i - 1, i),
    sepcol = "blue",
    key = TRUE,
    Rowv = FALSE,
    dendrogram = "col",
    margin = c(5, max(nchar(rownames(X2))) * (50 / 100)),
    lhei = c(p_key, p_heatmap)
  )
}
