data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNeverMindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage }
                             deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                       , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                      , OpenBSDPlusNeverMindJustBSDStill
                      , Mac
                      , Windows ]

allLanguages :: [ProgrammingLanguage]
allLanguages  = [ Haskell
                , Agda
                , Idris
                , PureScript ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]


