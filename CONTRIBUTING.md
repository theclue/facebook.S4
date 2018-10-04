# Contributing to facebook.S4

First of all, thanks for considering contributing to facebook.S4! It's people like you that make it rewarding for us - the project maintainers - to improve the package.

facebook.S4 is an open source project, maintained by people who care. We are not directly or indirectly funded to do so.

* [Repository](https://github.com/theclue/facebook.S4)
* [Issues](https://github.com/theclue/facebook.S4/issues)
* [New Issue](https://github.com/our_org/our_package/issues/new)
* __[Hackoctoberfest Issues](https://github.com/theclue/facebook.S4/labels/hacktoberfest)__ (fix and get rewarded, see [here](https://hacktoberfest.digitalocean.com/) for more details)
* [Wiki](https://github.com/theclue/facebook.S4) (under construction)

## How you can contribute

There are several ways you can contribute to this project. If you want to know more about why and how to contribute to open source projects like this one, see this [Open Source Guide](https://opensource.guide/how-to-contribute/).

### Share the love ❤️

Think our_package is useful? Let others discover it, by telling them in person, via Twitter or a blog post.

Using facebook.S4 for a paper you are writing? Consider citing it.

### Ask a question ⁉️

Using our_package and got stuck? Browse the [Wiki](https://github.com/theclue/facebook.S4) to see if you can find a solution. Still stuck? Post your question as an [issue on GitHub](https://github.com/our_org/our_package/issues/new). While we cannot guarantee user support, we'll try to do our best to address it, as questions often lead to better documentation or the discovery of bugs.

Want to ask a question in private? Contact the package maintainer on [https://gabrielebaldassarre.com][his website].

### Propose an idea 💡

Have an idea for a new our_package feature? Take a look at the Wiki and [issue list][issues] to see if it isn't included or suggested yet. If not, suggest your idea as an issue or perform a __pull request__. While we can't promise to implement your idea, it helps to:

* Explain in detail how it would work.
* Keep the scope as narrow as possible.

See below if you want to contribute code for your idea as well.

### Report a bug 🐛

Using facebook.S4 and discovered a bug? That's annoying! Don't let others have the same experience and report it as an issue so we can fix it. A good bug report makes it easier for us to do so, so please include:

* Your operating system name and version (e.g. Mac OS 10.13.6).
* Any details about your local setup that might be helpful in troubleshooting (for ex. R versions, system libraries and so on.).
* Detailed steps to reproduce the bug.

### Improve the documentation 📖

Noticed a typo on the website? Think a function could use a better example? Good documentation makes all the difference, so your help to improve it is very welcome!

#### Function documentation

Functions are described as comments near their code and translated to documentation using [`roxygen2`](https://klutometis.github.io/roxygen/). If you want to improve a function description:

1. Go to `R/` directory in the [code repository][repo].
2. Look for the file with the name of the function.
3. [Propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to update the function documentation in the roxygen comments (starting with `#'`).

### Contribute code 📝

Care to fix bugs or implement new functionality for our_package? Awesome! 👏 Have a look at the issue list and leave a comment on the things you want to work on. See also the development guidelines below.

## Development guidelines

We try to follow the [GitHub flow](https://guides.github.com/introduction/flow/) for development.

1. Fork [this repo][repo] and clone it to your computer. To learn more about this process, see [this guide](https://guides.github.com/activities/forking/).
2. If you have forked and cloned the project before and it has been a while since you worked on it, [pull changes from the original repo](https://help.github.com/articles/merging-an-upstream-repository-into-your-fork/) to your clone by using `git pull upstream master`.
3. Open the RStudio project file (`.Rproj`).
5. Make your changes:
    * Write your code.
    * Test your code (bonus points for adding unit tests).
    * Document your code (see function documentation above).
    * Do an `R CMD check` using `devtools::check()` and aim for 0 errors and warnings.
5. Commit and push your changes.
6. Submit a [pull request](https://guides.github.com/activities/forking/#making-a-pull-request).
