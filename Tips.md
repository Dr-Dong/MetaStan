---
title: "Tips"
author: "Burak Kürsad Günhan"
date: "March 7, 2018"
output: html_document
---

- Ironically, the cleanup (or cleanup.win on Windows) script — which is executed if you do R CMD build or R CMD INSTALL --preclean — calls tools/make_cpp, which creates src/include/models.hpp.

- Do not mess with make.cpp.R fle!

- My solution: 
  - Run makecpp.R manually
  - R CMD build MetaStan
  - R CMD INSTALL MetaStan_
But the problem is that it does not wotk with two Stan files!!!
