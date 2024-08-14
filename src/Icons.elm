module Icons exposing (..)

import Phosphor
import Ui


eye =
    Phosphor.eye Phosphor.Regular |> Phosphor.toHtml [] |> Ui.html |> Ui.el [ Ui.width Ui.shrink ]


eyeClosed =
    Phosphor.eyeClosed Phosphor.Regular |> Phosphor.toHtml [] |> Ui.html |> Ui.el [ Ui.width Ui.shrink ]


copy =
    Phosphor.copy Phosphor.Regular |> Phosphor.toHtml [] |> Ui.html |> Ui.el [ Ui.width Ui.shrink ]
