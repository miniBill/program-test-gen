module Icons exposing (eye, eyeClosed)

import Phosphor
import Ui


eye : Ui.Element msg
eye =
    Phosphor.eye Phosphor.Regular |> Phosphor.toHtml [] |> Ui.html |> Ui.el [ Ui.width Ui.shrink ]


eyeClosed : Ui.Element msg
eyeClosed =
    Phosphor.eyeClosed Phosphor.Regular |> Phosphor.toHtml [] |> Ui.html |> Ui.el [ Ui.width Ui.shrink ]
