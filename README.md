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

The main commands are:

` M-x js-import ` which allows selecting js modules from current project and package.json "dependencies".

` M-x js-import-dev ` which allows selecting js modules from current project and package.json "devDependencies".

Once called, select a dependency MODULE from the completing list presented and, depending on the number of prefix arguments (`C-u`) used to invoke the command:

- With no prefix argument, enter a NAME to be imported as the default export of the module, generating: ` import NAME from 'MODULE' `. If other exports from module are already imported, the NAME will be prepended to the existent import line. If the default module export has already been imported as another name, this will generate a new import with the chosen NAME.

- With one prefix argument, enter comma separated list of NAMES to import from module, generating: ` import { NAMES } from 'MODULE' `. If the module is already imported, the NAMES will be appended to the existent import line.

- With two prefix arguments, enter a NAME to import all exports from module, generating: ` import * as NAME from 'MODULE' `.

The default NAME to import is suggested to be either the symbol at point or the module name.

An import statement will be inserted after the other import statements in the file, or at top of the file if no import statements already exists.

[Blogpost](https://jakoblind.github.io/emacs/javascript/2016/10/16/automatically-import-js-files-from-you-project.html)

## Customization

You can use M-x customize-variable to modify generated import statements.

To choose the quoting style, set js-import-quote either to `"` or to `'`.

To switch between relative imports (`../foo`) and absolute imports from project root (`A/B/foo`), set js-import-style to either "relative" or "project-root".

