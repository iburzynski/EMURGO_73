# Creating a Haskell Project with Cabal
**Note:** you need to have `git` installed before proceeding further. To check if git is installed, open a terminal window and run the command:
```
git --version
```
If a version displays, proceed to the next section. Otherwise follow the installation instructions for your operating system here:
https://git-scm.com/book/en/v2/Getting-Started-Installing-Git

We won't cover the fundamentals of using Git and Github in this walkthrough.

If you're new to version control, you can follow one of these tutorials:

**VS Code Source Control method (recommended for beginners):** \
https://adamtheautomator.com/visual-studio-code-github-setup/

**Command Line method (if you prefer the terminal):** \
https://www.freecodecamp.org/news/git-and-github-for-beginners/

## I. Create Github Account & Create a New Repository
1. Go to https://github.com and create an account if you don't already have one. Otherwise log into your account.
2. Click the `+` icon in the top right corner and select `New Repository`.
3. Give your repository a name (the name of your project) and a short description (if desired).
4. Leave `Public` selected so your project can be reviewed by EMURGO faculty.
5. In the **Add .gitignore** section, select `Haskell`.
6. Optionally **Add a README file** as desired.
7. In the **Choose a license** section, leave `None` selected (we will create the license file when we initialize our Haskell project).
8. Click `Create repository` and you'll be taken to the repository page.
9. Install the `Github Pull Requests and Issues` extension (https://code.visualstudio.com/docs/sourcecontrol/github)

## II. Clone Repository
You can search for and clone a repository from GitHub in VS Code using the `Git: Clone` command in the Command Palette (`Ctrl+Shift+P`) or by using the `Clone Repository` button in the `Source Control` view (available when you have no folder open).

From the GitHub repository dropdown you can filter and pick the repository you want to clone locally.

### **Alternate Method Using Git CLI**
*Note: using the Git CLI with Github requires additional configuration that is not covered here. If you'd like to use Git/Github from the command line, you can follow the instructions for the HTTPS authentication method here: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/about-authentication-to-github#authenticating-with-the-command-line*

On your repository page, click the green `Code` button and copy the repository url under `HTTPS`.

In a terminal window, navigate to whichever directory you'd like your project directory to be placed, and enter the following command, pasting in the repository URL:

```
git clone https://github.com/<your username>/<your repository>.git
```

**Note:** VS Code frequently shows "module not found" errors in the `import` lines of project files when you have another folder (*i.e. a parent directory*) open in the Explorer instead of the project's root directory.

After cloning your project repository, use `File > Open Folder` and select the project root directory to open it in the Explorer. Do the same each time you open VS Code to work on your project.

Open a terminal window (`Ctrl` + `~` in VS Code) and verify that the path displayed in the terminal matches the project root directory before proceeding to the next section.

## III. Creating a Haskell Project with Cabal

*Reference: https://cabal.readthedocs.io/en/3.6/*

Cabal (Common Architecture for Building Applications and Libraries) is the standard package system for Haskell software.

If you installed Haskell using GHCUp, you should already have Cabal on your machine. To confirm, run the following command in a terminal window:

```
cabal -V
```

In your terminal window, make sure you are in the project root directory or else navigate there using the `cd` command.

Run the following command to download the latest package data from Hackage:

```
cabal update
```


Run the following command to start the interactive `cabal-install` tool:

```
cabal init -i
```

A series of interactive prompts will begin. Follow the responses listed after each prompt below:

```
Should I generate a simple project with sensible defaults? [default: y]
```

Type `n` and `Enter`.

```
What does the package build:
  1) Executable
  2) Library
  3) Library and Executable
```
Type `1` and `Enter`.

```
What is the main module of the executable:
 * 1) Main.hs (does not yet exist, but will be created)
   2) Main.lhs (does not yet exist, but will be created)
   3) Other (specify)
Your choice? [default: Main.hs (does not exist, but will be created)]
```

Hit `Enter` to accept the default.

```
Please choose version of the Cabal specification to use:
   1) 1.10   (legacy)
   2) 2.0    (+ support for Backpack, internal sub-libs, '^>=' operator)
   3) 2.2    (+ support for 'common', 'elif', redundant commas, SPDX)
 * 4) 2.4    (+ support for '**' globbing)
   5) 3.0    (+ set notation for ==, common stanzas in ifs, more redundant commas, better pkgconfig-depends)
Your choice? [default: 2.4    (+ support for '**' globbing)]
```

Hit `Enter` to accept the default.

```
Package name? [default: ...]
```

Type `Enter` to accept the default.

```
Package version? [default: 0.1.0.0]
```

Type `Enter` to accept the default.

```
Please choose a license:
 * 1) NONE
   2) BSD-2-Clause
   3) BSD-3-Clause
   4) Apache-2.0
   5) MIT
   6) MPL-2.0
   7) ISC
   8) GPL-2.0-only
   9) GPL-3.0-only
  10) LGPL-2.1-only
  11) LGPL-3.0-only
  12) AGPL-3.0-only
  13) GPL-2.0-or-later
  14) GPL-3.0-or-later
  15) LGPL-2.1-or-later
  16) LGPL-3.0-or-later
  17) AGPL-3.0-or-later
  18) Other (specify)
Your choice? [default: NONE]
```

Type the number corresponding to the license of your choice. We will use `3` (`BSD-3-Clause`) in this walkthrough.

```
Author name? [default: ...]
```

The name associated with your Github account should display as the default. Type `Enter` to accept.

```
Maintainer email? [default: ...]
```

The email associated with your Github account should display as the default. Type `Enter` to accept.

```
Project homepage URL?
```

Hit `Enter` to leave blank, or add the URL to the project repository on Github.

```
Project synopsis?
```

Hit `Enter` to leave blank, or type a short, one-line description of your project.

```
Project category:
```

Hit `Enter` for `(none)`, or choose the number associated with the most relevant category.

```
Application (Main.hs) directory:
 * 1) app
   2) src-exe
   3) (none)
   4) Other (specify)
Your choice? [default: app]
```

Hit `Enter` to accept the default.

```
What base language is the package written in:
 * 1) Haskell2010
   2) Haskell98
   3) Other (specify)
Your choice? [default: Haskell2010]
```

Hit `Enter` to accept the default.

```
Add informative comments to each field in the cabal file (y/n)? [default: n]
```

For your first project, type `y` and `Enter`, as these comments can help you to better understand the structure of the `.cabal` file. In future projects you can select `n`.

## IV. Adding Modules
It is best practice to keep the code in the `Main.hs` file to a minimum (i.e. limited to the `main` IO Action), and organize the rest of the application's code into separate modules.

You can do this by creating separate `.hs` files alongside `Main.hs` in your project's `app` directory.

One organization strategy could be to organize all of your custom data types in their own module file called `Types.hs`, additional IO Actions (besides `main`) in an `Actions.hs` file, application data in a `Data.hs` file, and all of the pure functions in one or more other modules. The way you organize your code is subjective and there isn't a single correct way to do it. However, your project should be well organized and adhere to the "onion architecture", with impure code (Actions) separated from pure code and kept to a minimum, with as much of your program logic written in pure functions as possible.

To make your additional files modules that can be imported and used in other application files, follow these steps:

1. Write a module declaration at the top of each file like this:

```haskell
module <ModuleName> where
```

Module names should begin with a capital letter and use camel-case if they contain multiple words (i.e. `GameUtils`). You can selectively export a limited selection of constants/functions/types/etc. by adding parentheses after the module name and listing them, separated by commas:

```haskell
module Foo (bar, baz, Quux) where
```

If you do not explicitly mention which items to export like this, everything in your module will be available in any file that imports it.

2. Find the `other-modules` property in the `executable` stanza of the `.cabal` file. Uncomment this line if it is commented out. Add the name of each module in your project (excluding the `.hs` file extension), separated by commas.

3. Import the module(s) in the `Main.hs` file via an import declaration, i.e.:

```haskell
import <ModuleName>
```

Just like we can selectively export items from a module, you can selectively import them by adding them (comma separated and parenthesized) after the module name in your import declaration, i.e.:

```haskell
import Foo (bar, baz)
```

If you do not include specific items like this, everything in the module will be imported and available for use inside the importing module.

## V. Adding Dependencies to .cabal File
In most real world Haskell projects, we'll wish to utilize third-party dependencies, or packages outside of "Base" (Haskell's standard library).

1. Search for the package you wish to use in your project on https://hackage.haskell.org/
2. Add the package name to the `build-depends` line of the `executable` stanza of your project `.cabal` file (after the `base` entry). Package names must be separated by commas, i.e.:
```
    build-depends:    base ^>=4.15.1.0,
                      random
```

3. Import the module name(s) at the top of your code file. Use the package's documentation on Hackage to find the module name(s). If the associated package has been added correctly you should not see any error displayed.

Here is an example of how we can import the `random` package into one of our modules:
```haskell
import System.Random
```

**Note on Hidden Packages:** many modules that are part of Base Haskell and can be imported without error in a vanilla `.hs` file will give `Could not load module` errors in a Cabal project.

This is because when Cabal asks GHC to build your package, it tells it to ignore all available packages except those explicitly listed in the `build-depends` of the `.cabal` file. The terminology GHC uses for this is "hidden".

Hidden packages need to be added to the `build-depends` line of the `.cabal` file. If your editor is configured with the relevant extensions, the error message should indicate the name of the library to add.

Some examples of commonly used "hidden" packages are:
* `containers` (includes common data structures like `Map`)
* `text` (includes the `Text` data type and utilities, which is more performant than `String` data in most cases)
* `mtl` (Monad Transformer Library: contains monad transformers and associated typeclasses)

## VI. Interactive Development with GHCi
To open a GHCi REPL with all of your project's dependencies available, enter the following command in the terminal (make sure you are in the project root directory):

```
cabal repl
```

You should see a prompt that says `Compiling Main` as well as any other modules used in your project. `Main` should appear in the GHCi prompt.

**Notes:**
* You must use the `cabal repl` command (not `ghci`) from inside the root directory (where the `.cabal` file is located) to properly run your project code in a REPL session. Using the regular `ghci` prompt will not load any of your project's external dependencies, so your code may not compile. Using the `cabal repl` command in any directory other than the root will not load any of your project modules.

* If you make changes to modules other than `Main`, those changes may not be reflected in your `Main` GHCi session, even if you use the `:r` command. You need to quit the GHCi session (`:q`) and run the `cabal repl` command again to start a fresh session.

* You can also switch to other modules in your project using the `:l` command followed by the module name, and use `:r` to reload the module after making changes. This is useful for testing individual modules as you're developing.

## VII. Saving Your Work
Make sure to stage, commit and push your changes as you go so you don't lose your work.

If you're new to Git/Github and not familiar with this workflow, follow one of the tutorials linked in the first section of this walkthrough.