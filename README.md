# js-import
Emacs package to automatically import JavaScript files from the current project or dependencies.

![Emacs js-import](https://jakoblind.github.io/img/jsimportnew.gif)

Select any JavaScript file from your project or dependency from package.json and automatically generate an import statement.

## Installation
It's available on [MELPA](https://melpa.org/)

```
M-x package-install js-import
```

Use [ido-ubiquitous](https://github.com/DarwinAwardWinner/ido-ubiquitous) to enable IDO

## Usage

The main command is

```
M-x js-import
```

Select a dependency from the list in the minibuffer. Then you need to enter a name of the import:
- Enter a comma separated list of member names, for example `a, b` generates `import { a, b } from "xx"`;
- Enter a name for the imported object, for example `a` generates `import a from "xx"`;
- Press enter to use default which is either the symbol at point, or the name of the dependency to be imported

An import statement will be inserted after the other import statements in the file, or at top of the file if no import statements already exists.

If you need a package defined in `devDependecies` section you can use:

```
M-x js-import-dev
```

[Blogpost](https://jakoblind.github.io/emacs/javascript/2016/10/16/automatically-import-js-files-from-you-project.html)
