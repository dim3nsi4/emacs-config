# emacs-config
Yet Another GNU Emacs Configuration.

The whole configuration is contained in the `init.el` file, which should be placed under `.emacs.d/` in your home directory. It relies on `use-package` to manage the loading of the packages and their configuration. Make sure you have it installed before trying to run this configuration.

Several packages are activated if they are detected (e.g. [ivy](https://github.com/abo-abo/swiper), [projectile](https://github.com/bbatsov/projectile), [company](https://github.com/company-mode/company-mode), ...). It also uses a custom theme
named `clearview-light`,  which must also be placed under `.emacs.d/` in your home directory. This theme can easily be customized using `M-x custom-visit-theme`.

For more details, see the [wiki](../../wiki) (under construction at the moment).

![alt tag](screenshot-emacs.png)
