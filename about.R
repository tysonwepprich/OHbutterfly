tabPanel("About",
         HTML(
           '<p style="text-align:justify">
           The <a href="http://www.ohiolepidopterists.org/" target="_blank">Ohio Lepidopterists</a> have 
            monitored sites across the state since 1995. This visualization would not be possible without
            the efforts of hundreds of volunteer observers. Learn more about butterfly monitoring in other states
            at <a href="http://www.nab-net.org/" target="_blank">The North American Butterfly Monitoring Network.</a>
            Methods for analyzing trends and phenology are based off those used by the <a href="http://www.ukbms.org/Methods.aspx" target="_blank">UK Butterfly Monitoring Scheme.</a>
           Climate data came from <a href="https://daymet.ornl.gov/" target="_blank">Daymet</a>.
          </p>'
         ),
         
         HTML('
              <p>Site by Tyson Wepprich<br/>
              <span class="spamspan">
              <span class="u">tyson.wepprich</span>
              [at]
              <span class="d">gmail [dot] com</span>
              </span><br/>
              <a href="https://twitter.com/TysonWepprich" target="_blank">@TysonWepprich</a> <br/>
              Please send me any comments or suggestions you have for this site.
              </p>'),
         

                  HTML('<strong>Visualization references</strong>
                       <p>
                        Adapted from <a href=http://shiny.snap.uaf.edu/cc4liteFinal/ target="_blank">Shiny app</a> by Matthew Leonawicz<br/>
                        Tutorial for Shiny Apps by Dean Attali <a href="http://deanattali.com/blog/building-shiny-apps-tutorial/" target="_blank">here.</a><br/>  
                       <a href="http://www.r-project.org/" target="_blank">Coded in R</a><br/>
                       <a href="http://www.rstudio.com/shiny/" target="_blank">Built with the Shiny package</a><br/>
                       <a href="http://rstudio.github.io/leaflet/" target="_blank">Maps with leaflet package</a><br/>
                        Source code at <a href="https://github.com/tysonwepprich/OHbutterfly" target="_blank">GitHub</a>
                       </p>')
                  
                  ,
         value="about"
                  )
