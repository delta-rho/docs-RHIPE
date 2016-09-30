
## install packages required to build the docs
#install.packages("devtools")
#library(devtools)
#install_github("staticdocs", "hadley")
#install_github("buildDocs", "hafen")


## load the pacakge
library(buildDocs)

my.buildref <- function (docsLoc, outLoc = NULL, pageList = NULL, navPill = "",
    editHref = "", copyrightText, windowTitle = NULL, root.dir = NULL,
    knit = TRUE, purl = TRUE)
{
    if (is.null(outLoc))
        outLoc <- docsLoc
    outFigLoc <- file.path(outLoc, "figures", "knitr/")
    if (!file.exists(outFigLoc))
        dir.create(outFigLoc, recursive = TRUE)
    outCacheLoc <- file.path(outLoc, "cache/")
    if (!file.exists(outCacheLoc))
        dir.create(outCacheLoc, recursive = TRUE)
    opts_knit$set(base.dir = outLoc)
    opts_chunk$set(comment = NA, tidy = FALSE, dpi = 150, base.dir = outLoc,
        fig.path = "figures/knitr/", cache.path = outCacheLoc)
    if (!is.null(root.dir))
        opts_knit$set(root.dir = normalizePath(root.dir))
    if (!file.exists(file.path(outLoc, "markdown")))
        dir.create(file.path(outLoc, "markdown"))
    if (!file.exists(file.path(outLoc, "code")))
        dir.create(file.path(outLoc, "code"))
    ff <- list.files(docsLoc, full.names = TRUE, pattern = "\\.Rmd$",
        recursive = TRUE)
    ff2 <- list.files(docsLoc, pattern = "\\.Rmd$", recursive = TRUE)
    if (!is.null(pageList)) {
        ff <- ff[ff2 %in% pageList]
    }
    else {
        pageList <- ff2
    }
    for (.file in ff) {
        ll <- readLines(.file)
        if (grepl("^\\`\\`\\`$", ll[1]))
            ll <- ll[-1]
        f2 <- textConnection(paste(ll, collapse = "\n"))
        if (purl)
            purl(f2, output = file.path(outLoc, "code", gsub("\\.Rmd",
                ".R", basename(.file))), documentation = 0)
        f2 <- textConnection(paste(ll, collapse = "\n"))
        if (knit)
            knit(f2, output = file.path(outLoc, "markdown", gsub("\\.Rmd",
                ".md", basename(.file))))
    }
    ff <- list.files(file.path(outLoc, "markdown"))
    if (length(ff) == 0)
        stop("There were no markdown files")
    ff <- ff[gsub("md$", "Rmd", basename(ff)) %in% basename(pageList)]
    aList <- suppressWarnings(lapply(file.path(outLoc, "markdown",
        ff), readLines))
    a <- do.call(c, aList)
    aFileIdx <- do.call(c, lapply(seq_along(aList), function(x) rep(x,
        length(aList[[x]]))))
    if (grepl("^\\`\\`\\`$", a[1]))
        a <- a[-1]
    knitrProblemInd <- which(grepl("\\\\```", a))
    if (length(knitrProblemInd) > 0) {
        a[knitrProblemInd] <- gsub("(.*)\\\\(```.*)", "\\1\\2",
            a[knitrProblemInd])
    }
    titleInd <- which(grepl("^# (.*) #$", a))
    if (length(titleInd) == 0) {
        pageTitle <- "No Title"
    }
    else {
        pageTitle <- gsub("^# (.*) #$", "\\1", a[titleInd])
    }
    if (is.null(windowTitle))
        windowTitle <- pageTitle
    a <- a[-titleInd]
    aFileIdx <- aFileIdx[-titleInd]
    chInd <- which(grepl("^## (.*) ##$", a))
    chNames <- gsub("^## (.*) ##$", "\\1", a[chInd])
    chFiles <- ff[aFileIdx[chInd]]
    chFiles <- gsub("\\.md$", ".Rmd", chFiles)
    if (is.null(editHref)) {
        chFileEditLink <- "#"
    }
    else {
        chFileEditLink <- paste(editHref, chFiles, sep = "")
    }
    a[chInd] <- ""
    chStart <- chInd
    chEnd <- c(chStart[-1] - 1, length(a))
    toc <- list()
    contents <- list()
    c_ind <- 1
    firstSection <- ""
    for (i in seq_along(chStart)) {
        b <- a[chStart[i]:chEnd[i]]
        secInd <- which(grepl("^### (.*) ###$", b))
        secNames <- gsub("^### (.*) ###$", "\\1", b[secInd])
        secID <- buildDocs:::validID(secNames)
        tmp <- paste(sprintf("\n      <li class='active'>\n         <a target='_self' class='nav-not-header' href='#%s'>%s</a>\n      </li>\n",
            secID, secNames), collapse = "\n")
        toc[[i]] <- sprintf("<li class='nav-header unselectable' data-edit-href='%s'>%s</li>\n      %s",
            chFileEditLink[i], chNames[i], tmp)
        secStart <- secInd
        secEnd <- c(secInd[-1] - 1, length(b))
        for (j in seq_along(secInd)) {
            if (c_ind == 1)
                firstSection <- secID[j]
            contents[[c_ind]] <- paste("<div class='tab-pane",
                ifelse(c_ind == 1, " active", ""), "' id='",
                secID[j], "'>\n", markdownToHTML(text = paste(b[secStart[j]:secEnd[j]],
                  collapse = "\n"), options = "fragment_only"),
                "\n</div>\n", sep = "")
            c_ind <- c_ind + 1
        }
    }
    toc <- paste(toc, collapse = "\n\n")
    contents <- paste(contents, collapse = "\n\n")
    if (!"package:buildDocs" %in% search()) {
        message("* ---- running dev version - getting templates from source")
        templatePath <- "~/Documents/Code/buildDocs/inst/templates"
        assetsLoc <- "~/Documents/Code/buildDocs/inst/assets"
    }
    else {
        templatePath <- file.path(system.file(package = "buildDocs"),
            "templates")
        assetsLoc <- file.path(system.file(package = "buildDocs"),
            "assets")
    }
    extraHeader <- "<script type=\"text/javascript\" src=\"assets/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">\n   MathJax.Hub.Config({    \n     extensions: [\"tex2jax.js\"],    \n     \"HTML-CSS\": { scale: 100}    \n   });\n   </script>"
    ll <- list(title = windowTitle, extra_css = "", navpills = navPill,
        header = pageTitle, side_span = "3", main_span = "9",
        toc = toc, content = contents, copyright = paste(copyrightText,
            ", ", format(Sys.time(), "%Y"), sep = ""), first_section = firstSection,
        extra_header = extraHeader)
    pageTemplate <- readLines(file.path(templatePath, "page_template.html"))
    b <- whisker.render(pageTemplate, ll)
    if (!file.exists(outLoc))
        dir.create(outLoc, recursive = TRUE)
    cat(b, file = file.path(outLoc, "functionref.html")) ##original is index.html
    digestMatch <- function(dirName, outLoc, assetsLoc) {
        d1 <- digest(as.character(md5sum(list.files(file.path(assetsLoc,
            dirName), recursive = TRUE, full.names = TRUE))))
        d2 <- digest(as.character(md5sum(list.files(file.path(outAssetsLoc,
            dirName), recursive = TRUE, full.names = TRUE))))
        d1 == d2
    }
    outAssetsLoc <- file.path(outLoc, "assets")
    if (!file.exists(outAssetsLoc))
        dir.create(outAssetsLoc)
    for (dirName in c("bootstrap", "custom", "font-awesome",
        "jquery", "MathJax", "prism", "svgeezy")) {
        if (!digestMatch(dirName, outAssetsLoc, assetsLoc)) {
            message("copying ", dirName)
            file.copy(file.path(assetsLoc, dirName), normalizePath(outAssetsLoc),
                overwrite = TRUE, recursive = TRUE)
        }
    }
}

my.buildref(
   docsLoc       = "./ref",
   outLoc        = ".",
   copyrightText = NULL,
   # pageList      = c("01.intro.Rmd", "02.wordcount.Rmd"),
   navPill       = packageNavPill("https://github.com/delta-rho/RHIPE", docsActive = FALSE),
   # editHref      = "https://github.com/user/project/edit/gh-pages/docs/",
   knit          = TRUE,
   purl          = FALSE
)

