tabPanel("About",
         HTML(
           '<p style="text-align:justify">Information can go here.</p>'),
         
         HTML('
              <p>Tyson Wepprich<br/>
              North Carolina State University<br/>
              <a href="https://twitter.com/TysonWepprich" target="_blank">@TysonWepprich</a> <br/>
              Adapted from <a href=http://shiny.snap.uaf.edu/cc4liteFinal/ target="_blank">Shiny app</a> by Matthew Leonawicz<br/>
              Tutorial for Shiny Apps by Dean Attali <a href="http://deanattali.com/blog/building-shiny-apps-tutorial/" target="_blank">here.</a>  
              </p>'),
         
         fluidRow(
           column(4,
                  HTML('<strong>References</strong>
                       <p></p><ul>
                       <li><a href="http://www.r-project.org/" target="_blank">Coded in R</a></li>
                       <li><a href="http://www.rstudio.com/shiny/" target="_blank">Built with the Shiny package</a></li>
                       <li>Additional supporting R packages</li>
                       <ul>
                       <li><a href="http://rstudio.github.io/shinythemes/" target="_blank">shinythemes</a></li>
                       <li><a href="https://github.com/ebailey78/shinyBS" target="_blank">shinyBS</a></li>
                       <li><a href="http://rstudio.github.io/leaflet/" target="_blank">plyr</a></li>
                       </ul>
                       <li>Source code on <a href="https://github.com/tysonwepprich/ohio_butterfly_dataviz.git" target="_blank">GitHub</a></li>
                       </ul>')
                  )
                  ),
         value="about"
                  )
