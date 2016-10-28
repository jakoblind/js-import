# js-import
Emacs package to automatically import JavaScript files from the current project or dependencies.

![Emacs js-import](https://jakoblind.github.io/img/emacs-import.gif)

Select any JavaScript file from your project or dependency from package.json and automatically generate an import statment at the point of the cursor.

## Installation
It's available on [MELPA](https://melpa.org/)

```
M-x package-install js-import
```

## Usage

```
M-x js-import
```

If you need a package defined in `devDependecies` section you can use:

```
M-x js-import-dev
```

[Blogpost](https://jakoblind.github.io/emacs/javascript/2016/10/16/automatically-import-js-files-from-you-project.html)
