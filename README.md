[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/facebook.S4)](http://cran.r-project.org/package=facebook.S4)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/facebook.S4)](https://github.com/metacran/cranlogs.app)

facebook.S4
===========

`facebook.S4` allows users to connect to Facebook through OAuth and get different kinds of content using the Facebook Graph API and organize it in a convenient set of S4 classes called *collections*. This allows to chain queries in the typical graph-oriented fashion of the Facebook API.

This project started as a fork of the [Rfacebook](https://github.com/pablobarbera/Rfacebook) package by Pablo Barbera, but it ended up as a complete rewrite, as I needed to better exploit the graph nature of the API and since I was a bit disappointed that the original package functions were not vectorized. Still, this package uses the `fbOAuth` function from Rfacebook, which is simply perfect.

Tutorials can be found at [https://gabrielebaldassarre.com/r/facebook.s4](https://gabrielebaldassarre.com/r/facebook.s4).

## Installation

Since the package is not yet available on CRAN, the fastest way to install it is pulling out the code from GitHub directly using the `devtools` package:
    
    devtools::install_github("theclue/facebook.S4")

## Authentication

Most API requests require the use of an access token.

There are two ways of making authenticated requests with `facebook.S4`. One option is to generate a temporary token on the [Graph API Explorer](https://developers.facebook.com/tools/explorer). Then just copy and paste the code into the R console and save it as a string vector to be passed as the `token` argument to any function or Collection constructor.While really easy, however, please note that this access token will only be valid for two hours. 

    library(facebook.S4)
    # token generated here: https://developers.facebook.com/tools/explorer 
    fb_token <- "XXXXXXXXXXXXXX"

It is possible to generate a 'long-lived' token (valid for two months) using the fbOAuth function, but the process is a bit longer. For a step-by-step tutorial, check this [setup tutorial](http://thinktostart.com/analyzing-facebook-with-r/).

## Package structure

There are two main kinds of elements in the package:

* The **Collections** - A set of S4 classes used, as the name suggests, to collect homogeneus items from Facebook. Their name is in the form of *Facebook&lt;Type&gt;Collection* (note the capital letter). Typical examples are: **FacebookPostsCollection**, **FacebookPagesCollection** and **FacebookCommentsCollection**.
* The **Finders** - A set of functions used to search elements on Facebook or inside a collection. They usually return simple lists, but some returns Collections, too. Typical examples are **facebook.search** and **facebook.video.formats**.

A special kind of collection called **FacebookMixedCollection** is used when you don't know in advance which kind of data you'll get from Facebook. For example, when you try to get a list of participants of a conversation, which can be *users* or *pages*.

While the set of Finders is very far to be considered as complete, the set of Collections covers already almost all the typical major items used in Facebook like posts, comments, likes, groups, albums and so on.

## First use: build a Facebook application

Before using this application, you need to build a Facebook application to authenticate `facebook.S4` against.

Since the procedure was covered many times, I'm not going to reinvent the wheel here. [This tutorial](https://github.com/pablobarbera/Rfacebook/wiki/How-to-get-started-with-RFacebook) will definitively help you, as facebook.S4 inherits the OAuth model from that package.

## Chaining a request

Almost all the Collections support chaining. It means, you can pass a collection as a source parameter to another one to chain complex requests. For example, let's say we want to get informations about who put a like on the feed for a set of pages:

    # Assuming fb_token is valid
    
    # First, we build the collection of pages. Since we're not interested in pages, we download no fields for pages
    fb.pages <- FacebookPagesCollection(id = c("9thcirclegames", "linuspage", fb_token, fields = ""))
    
    # Then, we pull out the last 20 posts for each page. Again, no fields here
    # When you pass another collection as source parameter, you can omit the token
    # (if you want to use the same, of course)
    fb.posts <- FacebookPostsCollection(fb.pages, fields = "", n = 20) 
    
    # Now, we're going to get all the likes. We want 'em all
    fb.likes <- FacebookLikesCollection(fb.posts, fields = "", n = Inf)
    
    # Finally, get the users
    fb.users <- FacebookUsersCollection(fb.likes, fields = c("id", "profile_type"))

You can eventually use the chain operator `%>%` for a more compact and elegant code:

    # Get the comments to the posts of the pages above
    pages %>% 
		FacebookPostsCollection(fields = "") %>%
     	FacebookCommentsCollection(fields = c("id", "from.fields(id,name)", "message", "created_time"))

Please note that if you don't specify the field argument, a default set of fields is pulled.

## Parent collections

As said before, you can build a collection passing a list of IDs of the same kind or from another collection.

If this is the case, two additional slots of the collections will be fed:

* **parent** - which the parent of the *i*th element of the collection (for ex. the post a comment belongs)
* **parent.collection** with the generating collection you can traverse

From the previous example, this is absolutely legit:

    # This returns a character vector with "9thcirclegames", "linuspage"
    fb.users@parent.collection@parent.collection@parent.collection
    
**Watch out the memory consumption!** Parent collections could be really memory consuming. Keep calm and do `gc()`!

## Methods available for collections

<table>
<colgroup>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
<col style="text-align:left;"/>
</colgroup>

<thead>
<tr>
  	<th style="text-align:left;">Method</th>
	<th style="text-align:left;">Effect on collection</th>
	<th style="text-align:left;">Note</th>

</tr>
</thead>

<tbody>
<tr>
	<td style="text-align:left;">[]</td>
	<td style="text-align:left;">Subsetting</td>
	<td style="text-align:left;">Filter by index or by ID, returns a Collection of the same kind</td>
</tr>
<tr>
	<td style="text-align:left;">c</td>
	<td style="text-align:left;">Combine</td>
	<td style="text-align:left;">Combine elements of two or more collections. Duplicated IDs are discarded only if the have different parents.</td>
</tr>
<tr>
	<td style="text-align:left;">as.list</td>
	<td style="text-align:left;">Convert to a list</td>
	<td style="text-align:left;">Return a list representation of a Collection</td>
</tr>
<tr>
	<td style="text-align:left;">as.data.frame</td>
	<td style="text-align:left;">Convert to a data.frame</td>
	<td style="text-align:left;">Return a data.frame representation of a Collection</td>
</tr>
<tr>
	<td style="text-align:left;">length</td>
	<td style="text-align:left;">Counts the elements</td>
	<td style="text-align:left;">Return the length of a collection</td>
</tr>
<tr>
	<td style="text-align:left;">summary</td>
	<td style="text-align:left;">Prints a summary</td>
	<td style="text-align:left;">Prints a summary view of the details of a collection</td>
</tr>
</tbody>
</table>

## Meta

* Please [report any issues or bugs](https://github.com/theclue/facebook.s4/issues).
* License: MIT
* Get citation information for `facebook.S4` in R doing `citation(package = 'facebook.S4')`
