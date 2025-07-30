
.pt <- 2.845276

theme_set(
  .theme()
  # theme_minimal(base_size = 14)+
  #   theme( panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank())+
  #   # theme( axis.text.y = element_blank())+
  #   .leg_tl
)

.gsv <- function( df ) { group_by( df, sjid, avisitn ) }
.gs  <- function( df ) { group_by( df, sjid          ) }
.ug  <- function( df ) { ungroup ( df                ) }
.tab <- function( df ) { df %>% table (exclude = F)    }

.find_non_numeric <- function(.data) {
  .data %>%
    filter(
      if_any(everything(),
             ~ !is.na(.) & is.na(suppressWarnings(as.numeric(.))))
    )
}

.fixmod <- function(df, d = 4) {
  threshold <- 10^(-d)
  
  df %>%
    mutate(
      p.value = ifelse(
        p.value < threshold,
        paste0("<", formatC(threshold, format = "f", digits = d)),
        formatC(p.value, format = "f", digits = d)
      )
    )
}

