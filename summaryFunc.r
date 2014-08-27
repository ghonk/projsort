summary.MI <- function (object, subset = NULL, ...) {
  if (length(object) == 0) {
    stop('Invalid input for "subset"')
  } else {
    if (length(object) == 1) {
      return(summary(object[[1]]))
    }
  }

  # Roman: This function isn't fecthing coefficients robustly. Something goes wrong. Contact package author. 
  getcoef <- function(obj) {
    # S4
    if (!isS4(obj)) {
      coef(obj)
    } else {
      if ("coef3" %in% slotNames(obj)) {
        obj@coef3
      } else {
        obj@coef
      }
    }
  }

    #
    res <- list()

    # Get indices
    subset <- if (is.null(subset)) {
      1:length(object)
    } else {
      c(subset)
    }

    # Compute the summary of all objects
    for (k in subset) {
      res[[k]] <- summary(object[[k]])
    }


    # Answer
    ans <- list(
      zelig = object[[1]]$name,
      call = object[[1]]$result@call,
      all = res
    )

    #
    coef1 <- se1 <- NULL

    #
    for (k in subset) {
#     tmp <-  getcoef(res[[k]]) # Roman: I changed this to coef, not 100% sure if the output is the same
      tmp <- coef(res[[k]])
      coef1 <- cbind(coef1, tmp[, 1])
      se1 <- cbind(se1, tmp[, 2])
    }

    rows <- nrow(coef1)
    Q <- apply(coef1, 1, mean)
    U <- apply(se1^2, 1, mean)
    B <- apply((coef1-Q)^2, 1, sum)/(length(subset)-1)
    var <- U+(1+1/length(subset))*B
    nu <- (length(subset)-1)*(1+U/((1+1/length(subset))*B))^2

    coef.table <- matrix(NA, nrow = rows, ncol = 4)
    dimnames(coef.table) <- list(rownames(coef1),
                                 c("Value", "Std. Error", "t-stat", "p-value"))
    coef.table[,1] <- Q
    coef.table[,2] <- sqrt(var)
    coef.table[,3] <- Q/sqrt(var)
    coef.table[,4] <- pt(abs(Q/sqrt(var)), df=nu, lower.tail=F)*2
    ans$coefficients <- coef.table
    ans$cov.scaled <- ans$cov.unscaled <- NULL

    for (i in 1:length(ans)) {
      if (is.numeric(ans[[i]]) && !names(ans)[i] %in% c("coefficients")) {
        tmp <- NULL
        for (j in subset) {
          r <- res[[j]]
          tmp <- cbind(tmp, r[[pmatch(names(ans)[i], names(res[[j]]))]])
        }
        ans[[i]] <- apply(tmp, 1, mean)
      }
    }

    class(ans) <- "summaryMI"
    ans
  }