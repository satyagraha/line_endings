# line_endings

Provides a report of line endings (and line ending anomalies) for a directory.

The primary application of this project is when preparing an Eclipse project for publication on GitHub. 
There are a fair number of issues which can arise when mixed or inconsistent line endings occur
in a project, and so this tool permits you to review your line endings usage and fix as necessary.

See [here](http://stackoverflow.com/questions/170961/whats-the-best-crlf-handling-strategy-with-git) 
for a good proposal on the best approach regarding Git line endings.

## Usage

The following instructions assume the use of Eclipse to build the project, but there is
no specific dependency on that IDE. You will need a Scala development environment, e.g.
[Scala IDE](http://scala-ide.org/). 

Proceed as follows:

* Clone locally https://github.com/lancegatlin/smach, import and build it as an Eclipse project
* Clone this project and import it into Eclipse
* Add the Smach project to this project's build path
* Create a Scala run configuration for the `Main` object and provide as an argument
the desired top-level directory to commence scanning
* Run the configuration to see the report

On all platforms, Eclipse aims to be line-ending agnostic and preserving, i.e. to read a file with
any line ending and to keep that line ending when the file is saved. However it is possible to
create files with inconsistent line endings and this may make it difficult for Eclipse to rightly
infer the file's line ending. Such inconsistencies are reported as well by this tool.  

## License

Eclipse Public License 1.0
