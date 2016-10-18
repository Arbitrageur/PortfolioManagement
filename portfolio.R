library(R6)
library(magrittr)
library(dplyr)
library(Rmpfr)

as.data.frame.mpfr <- function(x, row.names = NULL, stringsAsFactors = F, ...) {
  as.data.frame(formatMpfr(x), row.names = row.names, stringsAsFactors = F, ...)
}

can.be.number <- function(x) {
  idx <- grep("^[0-9.]+(e[0-9-]+)?$", x)
  ans <- logical(length(x))
  ans[idx] <- T
  return(ans)
}

PortfolioAbstract <-
  R6Class("PortfolioAbstract",
          public = list(
            cash = 0,
            data = NULL,
            ## weights = NULL,
            ## weights.mpfr = NULL,
            isValid = function() {
              sum(self$weights$weights) + self$cash == 1 &&
                (!self$allow.cash && self$cash == 0)
            },
            initialize = function(weights = NULL, cash = 0) {
              if (is.null(weights)) {
                stop(simpleError("Weights should be provided."))
              }
              names <- colnames(weights)
              if (!all(c("id", "weight") %in% names)) {
                stop(simpleError("The data must have 'id' and 'weight' fields."))
              }
              stopifnot(sum(weights$weight) == 1)
              ## weights <- weights[weights$weight > 0, ]
              tryCatch({
                weights$weight <- format(mpfr(weights$weight, 1024))
              }, error = function(e) stop(e))
              self$data <- weights[c('id', 'weight')]
            }
          ),
          private = list(
            allow.cash = F
          ),
          active = list(
            weights = function(w) {
              if (missing(w)) {
                tmp <- data.frame(id     = self$data$id,
                                  weight = as.numeric(mpfr(self$data$weight, 1024)))
                return(tmp)
              }
              stopifnot(is.data.frame(w))
              stopifnot(all(c('id', 'weight') %in% names(w)))
              if (is.numeric(w$weight) && sum(w$weight) == 1) {
                w$weight <- format(mpfr(w$weight, 1024))
                self$data <- w
              } else if (is.character(w$weight)) {
                tryCatch({
                  w$weight <- format(mpfr(w$weight, 1024))
                  self$data <- w
                }, error = function(e) stop(e))
              }
            }
          ))

Portfolio <-
  R6Class("Portfolio",
          public = list(
            data = NULL,
            summary = function() {
              numNames <- nrow(self$data)
              paste0("Portfolio Summary\n",
                     "=================\n\n",
                     ## "Market Value : ", format(round(self$nav/10^6), big.mark = ","), "M",
                     "# of Stocks : ", nrow(self$shares))
            },
            initialize = function(data) {
              ## Parameter checking
              stopifnot(!missing(data))
              stopifnot(is.data.frame(data))
              stopifnot('id' %in% names(data))
              stopifnot(all(c('shares', 'price') %in% names(data)) ||
                        all(c('mv', 'price') %in% names(data)))
              stopifnot(!any(is.na(data)))

              col <- c('id', 'shares', 'price', 'mv')
              available.col <- col[col %in% names(data)]
              self$data <- data[available.col]

              private$calcWeights()
            },
            invest = function(contribution = 0) {
              if (contribution == 0) {
                return(data.frame(id = character(0), trade = numeric(0), price = numeric(0)))
              }
              tmp <- self$data %>%
                mutate(trade = round(as.numeric(mpfr(weight, 1024) * contribution / price))) %>%
                dplyr::select(id, trade, price)
              return(tmp)
            },
            rebalance = function(target = NULL, contribution = 0, data = NULL) {
              # target must be of the class "Portfolio" or "PortfolioAbstract"
              if (is.null(target)) {
                if (contribution > 0) {
                  return(self$invest(contribution))
                } else {
                  stop(simpleError("Specify the target portfolio or the amount of contribution."))
                }
              } else if (!is(target, "PortfolioAbstract")) {
                stop(simpleError("Target must be a Portfolio."))
              }

              total <- self$nav + contribution
              if (is(target, "Portfolio")) {
                t <- target$data[c('id', 'price', 'weight') %in% names(target$data)] %>%
                  full_join(self$data[c('id', 'shares', 'price')], by='id') %>%
                  mutate(price = ifelse(is.na(price.x), price.y, price.x)) %>%
                  select(-price.x, -price.y) %>%
                  mutate(trade = round(as.numeric(mpfr(weight, 1024) * total / price))) %>%
                  dplyr::select(id, trade, price)
                return(t)
              } else {
                t <- target$data[c('id', 'weight') %in% names(target$data)] %>%
                  full_join(self$data[c('id', 'shares', 'price')], by='id') %>%
                  mutate_each(funs(replace(., is.na(.), 0))) %>%
                  mutate(trade = round(as.numeric(mpfr(weight, 1024) * total / price)) - shares) %>%
                  dplyr::select(id, trade, price)
                return(t)
              }
            },
            reevaluate = function(prices) {
              # Checking missing prices
              if (missing(prices)) {
                stop(simpleError("Price must be provided."))
              }
              if (!all(c('id', 'price') %in% names(prices))) {
                stop(simpleError("Price must contain the columns of 'id' and 'price'"))
              }
              ## missing.price <- self$data$id[!self$data$id %in% prices$id]
              ## if (length(missing.price) > 0) {
              ##   errors <- paste0("The price must be provided for all ids.\n", "\tMissing IDs : ",
              ##                    paste(missing.price, collapse = ", "))
              ##   message(errors)
              ## }

              # Reevaluate the portfolio
              self$data %<>% full_join(prices, by='id') %>%
                mutate(price = ifelse(is.na(price.y), price.x, price.y)) %>%
                select(-price.x, -price.y)

              private$calcWeights()
              invisible(self)
            },
            applyTrades= function(trades, amount = F) {
              # Checking the arguments
              if (!is.data.frame(trades)) {
                stop(simpleError("Trades must be a dataframe."))
              }
              if (!all(c("id", "trade", "price") %in% names(trades))) {
                stop(simpleError("Trades must contain the columns of 'id' and 'trade'."))
              }
              old.prices <- self$data[!self$data$id %in% trades[!is.na(trades$price), ]$id, ] %>%
                select(id, price)
              new.prices <- rbind(old.prices, trades[!is.na(trades$price), c('id', 'price')])

              if (amount) {
                trades.shares <- merge(trades[c('id', 'trade')],
                                       new.prices, all.x = T, by='id')
                trades.shares %<>% mutate(trade = round(trade / price))
                trades <- trades.shares
              }

              self$data %<>% select(id, shares, price) %>%
                full_join(trades, by='id') %>%
                mutate(price = ifelse(is.na(price.y), price.x, price.y)) %>%
                select(-price.x, -price.y) %>%
                mutate(shares = shares + trade) %>%
                select(-trade)
              private$calcWeights()
              invisible(self)

              ## tmp <- merge(self$shares, trades[c('id', 'trade')], by='id', all=T)
              ## tmp <- merge(tmp, new.prices, by='id', all=T)
              ## tmp[is.na(tmp)] <- 0
              ## tmp$shares <- tmp$shares + tmp$trade
              ## self$shares <- tmp[tmp$shares != 0, c('id', 'shares')]
              ## self$prices <- tmp[tmp$shares != 0, c('id', 'price')]
              ## private$calcWeights()
              ## invisible(self)
            }),
          private = list(
            allow.fraction = F,
            calcWeights = function() {
              if (all(c('shares', 'price') %in% names(self$data))) {
                stopifnot(!any(is.na(self$data[c('id', 'shares', 'price')])))

                self$data %<>%
                  mutate(mv = shares * price,
                         weight = format(mpfr(mv, 1024) / sum(mv))) %>%
                  filter(shares > 0)

              } else if (all(c('price', 'mv') %in% names(self$data))) {
                stopifnot(!any(is.na(self$data[c('id', 'price', 'mv')])))

                self$data %<>%
                  mutate(shares = round(mv / price),
                         weight = format(mpfr(mv, 1024) / sum(mv))) %>%
                  filter(shares > 0)
              }
            }
          ),
          active = list(
            nav = function() {
              return(sum(self$data$mv))
            },
            shares = function(d) {
              if (missing(d)) {
                return(self$data[c('id', 'shares')])
              }
            },
            prices = function(d) {
              if (missing(d)) {
                return(self$data[c('id', 'price')])
              }
            },
            mv = function(d) {
              if (missing(d)) {
                return(self$data[c('id', 'mv')])
              }
            }
          ),
          inherit = PortfolioAbstract)
