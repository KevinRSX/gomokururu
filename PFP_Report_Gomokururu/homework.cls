\LoadClass[12pt]{article}
\ProvidesClass{homework}

\RequirePackage{fancyhdr}
\RequirePackage{amsmath,amsthm,amsfonts,amssymb}
\RequirePackage[letterpaper,margin=1in]{geometry}
\usepackage[parfill]{parskip}
\usepackage{makecell}

\newcommand{\studname}[1]{\gdef\@studname{#1}}
\newcommand{\studmail}[1]{\gdef\@studmail{#1}}
\newcommand{\coursename}[1]{\gdef\@coursename{#1}}
\newcommand{\teamname}[1]{\gdef\@teamname{#1}}
\newcommand{\reportname}[1]{\gdef\@reportname{#1}}


\let\Oldmaketitle\maketitle
\renewcommand{\maketitle}
{
	\Oldmaketitle
	\thispagestyle{plain}
}

\fancypagestyle{plain}
{
	\fancyhead{}
	\fancyfoot{}
% 	\fancyfoot[LE,LO]{\@teamname}
	\fancyfoot[RE,RO] {\thepage}
	\renewcommand{\headrulewidth}{0pt}
}

\pagestyle{fancy}

\fancyhead{}
\fancyfoot{}
\fancyhead[RE,RO]{\@reportname}
\fancyhead[LE,LO]{\@coursename}
\fancyfoot[RE,RO] {\thepage}
% \fancyfoot[LE,LO]{\@teamname}
\renewcommand{\footrulewidth}{0.4 pt}

\title{\@coursename \\ \@reportname}
\author{
   \@studname
  \\
  \@studmail
}
\date{\today}