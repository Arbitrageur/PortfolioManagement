library(R6)
library(dplyr)
library(magrittr)

PortfolioAbstract <-
  R6Class("PortfolioAbstract",
          public = list(
            cash = 0,
            weights = NULL,
            weights.mpfr = NULL,
            isValid = function() {
              sum(self$weights$weights) + self$cash == 1 &&
                (!self$allow.cash && self$cash == 0)
            },
            initialize = function(weights = NULL, cash = 0) {
              if (is.null(weights)) {
                stop(simpleError("Weights should be provided."))
              }
              names <- colnames(weights)
              if (!all(c("id", "weights") %in% names)) {
                stop(simpleError("The weights have to have 'id' and 'weights' fields."))
              }
              weights <- weights[weights$weights > 0, ]
              self$weights.mpfr <- self$weights <- weights[c('id', 'weights')]
              self$weights.mpfr$weights <- format(self$weights.mpfr$weights)
            }
          ),
          private = list(
            allow.cash = F
          ))

Portfolio <-
  R6Class("Portfolio",
          public = list(
            shares     = NULL,
            prices     = NULL,
            mv         = NULL,
            nav        = 0,
            summary    = function() {},
            initialize = function(shares = NULL, prices = NULL, mv = NULL, nav = 0, weights = NULL) {
                                        # Parameter checking
              if (!is.null(shares) && !is.null(prices)) {
                                        # Required columns check
                if (!all(c('id', 'shares') %in% names(shares))) {
                  stop(simpleError("The shares must have the columns of 'id' and 'shares'"))
                }
                if (!all(c('id', 'price') %in% names(prices))) {
                  stop(simpleError("The prices must have the columns of 'id' and ''"))
                }
                missing.id <- shares$id[!shares$id %in% prices$id]
                if (length(missing.id) > 0) {
                  stop(simpleError(paste0("The prices for these ids are required.\n",
                                          do.call(paste, as.list(c(missing.id, sep=","))))))
                }
                self$shares <- shares[c('id', 'shares')]
                self$prices <- prices[c('id', 'price')]
                private$calcWeights()
              }
            },
            invest = function(contribution = 0) {
              if (contribution == 0) {
                return(data.frame(id = character(0), trade = numeric(0)))
              } else {
                tmp <- merge(self$weights.mpfr, self$prices, by = 'id', all.x = T)
                tmp <- merge(tmp, self$shares, by = 'id', all.x = T)
                tmp.weights <- mpfr(tmp$weights, precBits = 1024)
                tmp.mv <- tmp.weights * (self$nav + contribution)
                tmp.shares <- tmp.mv / tmp$price
                trades <- round(as.numeric(tmp.shares)) - tmp$shares
                self$weights.mpfr <- data.frame(id      = tmp$id,
                                                weights = formatMpfr(tmp.weights))
                return(data.frame(id = tmp$id, trade = trades))
              }
            },
            rebalance = function(target = NULL, contribution = 0, data = NULL) {
              # target must be of the class "Portfolio" or "PortfolioAbstract"
              if (!is(target, "PortfolioAbstract")) {
                stop(simpleError("Target must be a Portfolio."))
              }

              prices <- rbind(target$prices, self$prices)
              if (!is.null(data) && all(c('id', 'price') %in% names(data))) {
                prices <- rbind(prices, data[c('id', 'price')])
              }
              missing.price <- target$weights$id[!all(target$weights$id %in% prices$id)]
              if (length(missing.price) > 0) {
                errors <- paste0("The price must be provided for all ids.\n", "\tIDs : ",
                                 do.call(paste, as.list(c(missing.price, sep=", "))))
                stop(simpleError(errors))
              }

              tmp <- merge(target$weights.mpfr,
                           prices,
                           by = 'id', all.x = T)

              tmp <- merge(tmp,
                           self$shares,
                           by = 'id', all = T)
              tmp[is.na(tmp)] <- 0
              tmp.weights <- mpfr(tmp$weights, precBits = 1024)
              tmp.mv <- tmp.weights * (self$nav + contribution)
              tmp.shares <- tmp.mv / tmp$price
              tmp.shares[is.nan(tmp.shares)] <- 0
              self$weights.mpfr <- data.frame(id      = tmp$id,
                                              weights = formatMpfr(tmp.weights))
              trades <- round(as.numeric(tmp.shares)) - tmp$shares
              return(data.frame(id = tmp$id, trade = trades, price = tmp$price))
            },
            reevaluate = function(prices) {
              # Checking missing prices
              if (missing(prices)) {
                stop(simpleError("Price must be provided."))
              }
              if (!all(c('id', 'price') %in% names(prices))) {
                stop(simpleError("Price must contain the columns of 'id' and 'price'"))
              }
              missing.price <- self$shares$id[!all(self$shares$id %in% prices$id)]
              if (length(missing.price) > 0) {
                errors <- paste0("The price must be provided for all ids.\n", "\tMissing IDs : ",
                                 do.call(paste, as.list(c(missing.price, sep=", "))))
                stop(simpleError(errors))
              }

              # Reevaluate the portfolio
              self$prices <- prices
              private$calcWeights()
              invisible(self)
            },
            applyTrades= function(trades) {
              # Checking the arguments
              if (!is.data.frame(trades)) {
                stop(simpleError("Trades is not data.frame."))
              }
              if (!all(c("id", "trade") %in% names(trades))) {
                stop(simpleError("Trades must contain the columns of 'id' and 'trade'."))
              }
              old.prices <- self$prices[!self$prices$id %in% trades$id, ]
              new.prices <- rbind(old.prices, trades[c('id', 'price')])

              tmp <- merge(self$shares, trades[c('id', 'trade')], by='id', all=T)
              tmp <- merge(tmp, new.prices, by='id', all=T)
              tmp[is.na(tmp)] <- 0
              tmp$shares <- tmp$shares + tmp$trade
              self$shares <- tmp[tmp$shares != 0, c('id', 'shares')]
              self$prices <- tmp[tmp$shares != 0, c('id', 'price')]
              private$calcWeights()
              invisible(self)
            }),
          private = list(
            allow.fraction = F,
            calcWeights = function() {
              tmp <- merge(self$shares, self$prices, by = 'id', all.x = TRUE)
              tmp$mv <- tmp$shares * tmp$price
              self$mv <- tmp[c('id', 'mv')]
              self$nav <- sum(self$mv$mv)
              tmp$weights <- tmp$mv / self$nav
              super$initialize(tmp[c('id', 'weights')])
            }
          ),
          active = list(
          ),
          inherit = PortfolioAbstract)
