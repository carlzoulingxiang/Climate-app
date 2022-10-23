function(){
	tabPanel("About",
		HTML("<h1> Ling Xiang(Carl),Zou</h1>
        <p>My name is Carl, a student at SFU majoring in statistics and minoring in mathematics. The programming language I mainly use is R, and also have some experience in Python and SAS. I have two years of statistical knowledge background and some experience on prediction model buildings, estimation, statistical analysis, and data visualization. </p>
        <p >In this project, I will explain how climate is changing in as time goes by in Vancouver with my analysis. The project is divided into four tabs; the 'User' tab will provide some information about the climate and allow a lot of flexibility for the users that include to try out different datasets, plots, and analyses. The 'Findings' tab will explain my findings by illustrating with the plots. I will make comparisons with different datasets with the variable, year, and make a reasonable prediction of what the climate will be in the future.</p>"
		
		),#end of html part 1.
        #Notice that I used double quotes (") above because otherwise it would interfere with
        # the single quote in the word (don't)
        HTML('
        
        <p>
        
        lza109@sfu.ca <br/>
        <a href="https://www.linkedin.com/in/carl-zou-300061174/" target="_blank">Linkedin</a> <br/>
        </p>'),
		value="about"
	)
}
