# prf-theme

A simple theme switcher for Emacs.

Was made originally to work with now legacy `color-theme`, and got updated to support `deftheme` and legacy  (Emacs pre-24).

It's certainly not the best implementation there is.


## Installation

Not on [Melpa](https://melpa.org/).

For now, the recommended way to install is with [use-package](https://github.com/jwiegley/use-package), [quelpa](https://github.com/quelpa/quelpa) and [quelpa-use-package](https://github.com/quelpa/quelpa-use-package).

```el
(use-package prf-theme
  :quelpa (prf-theme :fetcher github :repo "p3r7/prf-theme"))
```

## Configuration

Configure var `prf/theme/theme-list` with the list of themes you want to cycle through.

The the can also be updated at run time and would start back at the fiest theme in this case.

Then call `(prf/theme/initialize)` to activate the first theme in your list.

To cycle bewteen themes, call `prf/theme/cycle-theme`.
For conveniece, you can bind it to a key.

If you load [helm](https://github.com/emacs-helm/helm), addtionnal command `helm-prf-theme-choose` will let you select the theme to switch to from the list.


#### Sample config

```el
;; NB: use defer to prevent theme to get applied at init time
(use-package chocolate-theme
  :defer t)
(use-package white-sand-theme
  :defer t)

(use-package prf-theme
  :quelpa (prf-theme :fetcher github :repo "p3r7/prf-theme"))
  :demand

  :bind ([f12] . prf/theme/cycle-theme)

  :init
  (setq prf/theme/theme-list '(chocolate white-sand))

  :config
  (prf/theme/initialize))
```

## Implementation details

#### Cycling

The implementation is the most straightforward way to do it.

A copy of configured `prf/theme/theme-list` is created: `prf/theme/current-theme-list`.

We cycle through it by popping out the first element (the _car_) and setting it as the current theme.

Once `prf/theme/current-theme-list` is empty, the cycle back by re-setting it to the value of `prf/theme/theme-list`.


#### Unloading / Loading theme

For `deftheme`, we have first to `disable-theme` the previous one to then `load-theme` the next. Otherwise faces from previous theme would be kept.

Legacy `color-theme` represent themes as functions. To load one we'd need to just `funcall` it. There is no need to unload the previous theme.


### `deftheme` / `color-theme` abstraction

Abstraction of the theme implementation is achieved by defining function aliases.

```el
(if (>= emacs-major-version 24)
    (progn
      (defalias 'prf/theme/apply-theme #'prf/deftheme/apply-theme)
      (defalias 'prf/theme/revert-theme #'prf/deftheme/revert-theme))
  (defalias 'prf/theme/apply-theme #'prf/color-theme/apply-theme)
  (defalias 'prf/theme/revert-theme #'prf/color-theme/revert-theme)
  (require 'color-theme)
  (with-eval-after-load "color-theme"
    (color-theme-initialize))
  (setq color-theme-is-global     t
        color-theme-is-cumulative t))
```


## Similar projects

 - [https://github.com/myTerminal/theme-looper](myTerminal/theme-looper)
 - Commands `doremi-color-themes+` and `doremi-custom-themes+` from [DoReMi](https://www.emacswiki.org/emacs/DoReMi)
