Study Design Webapp Overview -------------------------------------------------------

This project is designed to provide an application that helps in the analysis of specific
study designs with an emphasis on public health research in rural health departments.
This provides a RShiny app with a user-friendly UI and guide that
can automatically perform basic statistical analyses.

How to Run -------------------------------------------------------------------------

Navigate to /DANA/DANA.Rproj and open the file in Rstudio. Within Rstudio,
select file>open>webapp/server. In the console,
load the dana package (needed for webapp functionality) by typing
devtools::load_all("dana"). From there, click the 'Run App' button or
type Shiny::runApp() in the console, which will launch the webapp in a separate
window.

Organization -----------------------------------------------------------------------

Each of the entries below is a subfolder or file in the main directory dana/:

dana - This is an associated R package designed mainly for use within the 
webapp. 

data - Contains example and real-world data, primarily for testing purposes.

demos - Contains various files used in testing the statistical performance
of model diagnostics. In particular, contains testing documents for 
goodness-of-fit measures used in model cascade.

dendrogram - Various files used for planning the implemented dendrogram.

webapp - This is the main chunk of code used to run the webapp. Format
is a 3-document Shiny application, with a subfolder for static images.

Development Workflow --------------------------------------------------------------

This is a brief overview of workflow for myself and anyone else who works on this
project.

Basic Git Workflow:

Abridged Setup

In Git terminal, generate SSH key pair on local machine, and add the public
key to your GitHub account. Do not share the private key! (this should always remain local).
This ensures GitHub can authenticate push changes. 

From there, clone and remote into the repo. Create a branch and use this
branch as you add features, and periodically merge into the main branch.

Updates a branch named MyBranch.

1. cd ../path/to/../DANA
2. git checkout MyBranch (switches to MyBranch)
3. git branch (check you're on correct branch, optional but good practice)
4. git status (check changes, optional but good practice)
5. Make Changes
6. git add . (stage all changes)
7. git commit -m "comment your changes" (commit changes)
8. git push -u origin MyBranch (set upstream, allows git push without extra arguments. Used first time you create a branch)

Pushes changes to master branch.

1. cd path/to/../DANA
2. git checkout master
3. git pull (update master locally)
4. git checkout MyBranch
5. git merge master
6. Make Changes
7. git add -A
8. git commit -m"comment your changes"
9. git checkout master
10. git pull
11. git merge MyBranch (merges MyBranch onto master)
12. git push



Updating Dana:

See here for a useful guide: https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html



Additional Notes:

I recommend opening up the webapp study_design_webapp.Rproj and also the dana.Rproj
files in different windows for development and testing.

On the webapp study_design_webapp.Rproj, the server file contains most of the code to
run the shiny app. To test changes, click the "run app" button.

Create .R files in the /data/R folder. Each .R file may or may not
contain multiple functions. Best practice is to put related
functions in the same .R file.
	Note that any functions requiring an external package dependency should
	be specified with the :: operator. For instance dplyr::mutate(). If
	you use one of these, add the required package in the /DESCRIPTION file
	under the Imports: section.
Use the roxygen2 documentation setup in the .R file. This has arguments
like @param and @return to specify the documentation typically seen
when you call ? or help() on a regular R function.

Once documentation is complete for the function,
run devtools::document() to automatically generate
a .Rd file in the /man folder, corresponding
to the function you just defined. This process is automatic
and no need to manually modify this .Rd file. Each time documentation
is added, you need to run devtools::document()

To store data available to package users, call devtools::use_data()
on a R object. This saves the object into data/x.rda. To provide
the code used to generate the object, ensure that ^data-raw$ is in
the .Rbuildignore, and ensure that the data-raw folder contains
the code.

To access the package, use devtools::load_all(), loading
R into memory. To ensure this is available outside of that
particular R session, run devtools::install() which installs 
the package into the R library. then do library("mypackage")
to access everything.

If permission is denied even after keygen, try running the agent:

eval "$(ssh-agent -s)"
ssh-add ~/.ssh/path/to/key








