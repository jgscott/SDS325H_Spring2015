---
layout: page
title: Resources
---

Course packet
-------------

This is in a state of continual editing, so I will post the
  chapters as they become relevant. 

* [Introduction](files/files/00-FrontMatter-Intro.pdf)  
* [Chapter 1](files/01-ExplanationsAndEvidence.pdf)


Other readings
--------------

* [Data Analysis for Politics and Policy](files/DAPP.pdf) by Edward Tufte.  Out of print, but available for free in electronic form. I will often assign supplemental readings out of this book.  
* [Statistical Modeling: A Fresh Approach](http://www.mosaic-web.org/go/StatisticalModeling/Chapters/) by Daniel Kaplan.  Great book, great exercises.  The first five chapters are available for free.  You will find the first one especially helpful in learning some basic R commands.  You are not required to buy this book, but you may find it a useful resource. Readings will be assigned out of the first five (free) chapters.
* [Using R for Introductory Statistics](http://cran.r-project.org/doc/contrib/Verzani-SimpleR.pdf).  A useful guide on doing basic statistics in R.


Good places to get help with R
------------------------------
*[R Seek](http://rseek.org).  A front-end for Google that helps get more Relevant results.
*[Stack Overflow](http://stackoverflow.com). Just search for a relevant question and somebody has probably answered it before.


Scribe reports
--------------
Below are your classmates' notes, day by day.

<ul>
{% for ex in site.data.scribe %}
  <li>
    <a href="files/scribe/{{ ex.scribe }}">
      {{ ex.scribe }}
    </a>
  </li>
{% endfor %}
</ul>
