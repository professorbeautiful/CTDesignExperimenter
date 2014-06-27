if( interactive()  & ( Sys.getenv("USER") == "Roger"))
{
	### In RStudio,  .Rprofile does work. .First does not.
	#sink("/dev/null")
	cat("Running .Rprofile from the CTDE project.\n")
	if(length(grep("package:mvbutils", search())) == 0) 
		library("mvbutils")
	if(length(grep("R-utilities", search())) == 0) 
		attach("~/Dropbox/_HOME/R-in-Dropbox/R-utilities/.RData")
	require("utils")
	## error: could not find function "packageDescription" because utils was not attached.

	options(repos="http://cran.r-project.org")
	options(texi2dvi="pdflatex")
	options(stringsAsFactors = FALSE)

	if(Sys.getenv("R_USER") ==  "")
	  Sys.setenv(R_USER=Sys.getenv("HOME"))

	options(program.editor = function(name, fname) paste("open ", 
	    fname, "", sep = ""), edit.scratchdir = "/Users/Roger/R.scratch", 
	    backup.fix = c(2L, 2L))
	autoedit( TRUE)
	options( mlazy.index=TRUE)
	options(program.editor = function(name, fname)
	  paste("open ", fname, "", sep = ""),
		edit.scratchdir = "~/R.scratch", backup.fix = 2:3)
	options(menu.graphics = TRUE)
	options(write.mvb.tasks = TRUE)
	#NO LONGER NEEDED!  assign.to.base("help", base.help, override.env = FALSE)
	Sys.setenv(DISPLAY = ":0")
	abbreviate.cdprompt = 3
	Sys.setlocale("LC_COLLATE", "en_US.UTF-8")

	options(devtools.install.args="--no-multiarch")
}
