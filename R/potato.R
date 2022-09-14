#' Start a Game of 'Potato'
#'
#' @description
#' Begin an interactive version of 'Potato', a one-page RPG by Oliver Darkshire.
#' Messages and prompts are printed to the console.
#'
#' @source https://www.patreon.com/deathbybadger
#'
#' @return Nothing.
#'
#' @export
#'
#' @examples /dontrun{potato()}
potato <- function() {

  message("--- POTATO ---")

  message("\nA (one-page) RPG by Oliver Darkshire (@deathbybadger)")
  message("This and more at https://www.patreon.com/deathbybadger")

  message("\nYou are a halfling, just trying to exist.")
  message("Meanwhile, the dark lord rampages across the world.")
  message("You do not care about this. You are trying to farm potatoes.")
  message("Because what could a halfling possibly do about it anyway?")

  message("\nKeep going until DESTINY, POTATO or ORC reach 10/10.")

  DESTINY <- 0L
  POTATO  <- 0L
  ORC     <- 0L
  COST    <- 1L

  can_pay  <- FALSE

  repeat {

    message(paste0("\n- DESTINY: ", DESTINY, "/10"))
    message(paste0("- POTATO:  ", POTATO, "/10"))
    message(paste0("- ORC:     ", ORC, "/10"))
    message(paste0("- PAY:     ", COST, " POTATO to remove 1 ORC\n"))

    if (ORC >= 10L) {

      message("- ORC reached 10!")
      message("- Orcs finallv find your potato farm. Alas, orcs are not so interested in potatoes as they are in eating you, and you end up in a cookpot.")

      message("\n--- GAME OVER ---")

      break

    } else if (POTATO >= 10L) {

      message("- POTATO reached 10!")
      message("- You have enough potatoes that you can go underground and not return to the surface until the danger is past. You nestle down into your burrow and enjoy your well earned rest.")

      message("\n--- GAME OVER ---")

      break

    } else if (DESTINY >= 10L) {

      message("- DESTINY reached 10!")
      message("- An interfering bard or wizard turns up at your doorstep with a quest and you are whisked away against your will on an adventure (unless you've already been eaten by orcs).")

      message("\n--- GAME OVER ---")

      break

    }

    if (COST <= POTATO & ORC >= 1) {
      can_pay <- TRUE
    } else if (COST > POTATO) {
      can_pay <- FALSE
    }

    if (can_pay) {

      input_okay <- FALSE

      while (!input_okay) {

        event <- readline(
          paste(
            "Press [ENTER] to roll or [p] to pay",
            COST,
            "POTATO to remove 1 ORC... "
          )
        )

        if (event %in% c("", "p")) {
          input_okay <- TRUE
        }

      }

    } else if (!can_pay) {

      input_okay <- FALSE

      while (!input_okay) {

        event <- readline("Press [ENTER] to roll... ")

        if (event == "") {
          input_okay <- TRUE
        }

      }

    }

    if (event == "p") {

      message(paste("\n- Hurled", COST, "POTATO to scare off 1 ORC"))

      POTATO <- POTATO - 1L
      ORC <- ORC - 1L

    } else if (event == "") {

      rolled <- .roll()
      rolled_msg <- paste0("\n- Rolled ", rolled, ":")

      if (rolled %in% 1:2L) {

        message(paste(rolled_msg, "In the garden..."))

        rolled_garden <- .roll()
        rolled_garden_msg <- paste0("- Rolled ", rolled_garden, ":")

        if (rolled_garden == 1L) {

          message(paste(rolled_garden_msg, "You happily root about all day in your garden."))
          message("- Result: +1 POTATO")

          POTATO <- POTATO + 1L

        }

        if (rolled_garden == 2L) {

          message(paste(rolled_garden_msg, "You narrowly avoid a visitor by hiding in a potato sack."))
          message("- Result: +1 POTATO, +1 DESTINY")

          POTATO <- POTATO + 1L
          DESTINY <- DESTINY + 1L

        }

        if (rolled_garden == 3L) {

          message(paste(rolled_garden_msg, "A hooded stranger lingers outside your farm."))
          message("- Result: +1 DESTINY, +1 ORC")

          DESTINY <- DESTINY + 1L
          ORC <- ORC + 1L

        }

        if (rolled_garden == 4L) {

          message(paste(rolled_garden_msg, "Your field is ravaged in the night by unseen enemies."))
          message("- Result: +1 ORC, -1 POTATO")

          ORC <- ORC + 1L
          POTATO <- POTATO - 1L

        }

        if (rolled_garden == 5L) {

          message(paste(rolled_garden_msg, "You trade potatoes for other delicious foodstuffs."))
          message("- Result: -1 POTATO")

          POTATO <- POTATO - 1L

        }

        if (rolled_garden == 6L) {

          message(paste(rolled_garden_msg, "You burrow into a bumper crop of potatoes. Do you cry with joy? Possibly."))
          message("- Result: +2 POTATO")

          POTATO <- POTATO + 2L

        }

      } else if (rolled %in% 3:4L) {

        message(paste(rolled_msg, "A knock at the door... "))

        rolled_knock <- .roll()
        rolled_knock_msg <- paste0("- Rolled ", rolled_knock, ":")

        if (rolled_knock == 1L) {

          message(paste(rolled_knock_msg, "A distant cousin. They are after your potatoes. They may snitch on you."))
          message("- Result: +1 ORC")

          ORC <- ORC + 1L

        }

        if (rolled_knock == 2L) {

          message(paste(rolled_knock_msg, "A dwarven stranger. You refuse them entry. Ghastly creatures."))
          message("- Result: +1 DESTINY")

          DESTINY <- DESTINY + 1L

        }

        if (rolled_knock == 3L) {

          message(paste(rolled_knock_msg, "A wizard strolls by. You pointedly draw the curtains."))
          message("- Result: +1 ORC, +1 DESTINY")

          ORC <- ORC + 1L
          DESTINY <- DESTINY + 1L

        }

        if (rolled_knock == 4L) {

          message(paste(rolled_knock_msg, "There are rumours of war in the reaches. You eat some potatoes."))
          message("- Result: -1 POTATO, +2 ORC")

          POTATO <- POTATO - 1L
          ORC <- ORC + 2L

        }

        if (rolled_knock == 5L) {

          message(paste(rolled_knock_msg, "It's an elf. They are not serious people."))
          message("- Result: +1 DESTINY")

          DESTINY <- DESTINY + 1L

        }

        if (rolled_knock == 6L) {

          message(paste(rolled_knock_msg, "It's a sack of potatoes from a generous neighbour. You really must remember to pay them a visit one of these years."))
          message("- Result: +2 POTATO")

          POTATO <- POTATO + 2L

        }

      } else if (rolled %in% 5:6L) {

        message(paste(rolled_msg, "The world becomes a darker, more dangerous place."))
        message("- Result: From now on, removing ORC costs an additional POTATO (this is cumulative)")

        COST <- COST + 1L

      }

      if (DESTINY < 0L)  {
        DESTINY <- 0L
      } else if (POTATO < 0L) {
        POTATO <- 0L
      } else if (ORC < 0L) {
        ORC <- 0L
      }

    }

  }

}
