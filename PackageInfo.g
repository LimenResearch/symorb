SetPackageInfo( rec(

  PackageName := "symorb",
  

  Subtitle := "symorb/Minimizing orbits",


  # copiato versione da Davide
  Version := "0.92",
  

  # coiato data da Davide
  Date := "20/06/2005",
  

  ## qui ho messo il git di symorb
  PackageWWWHome :=
    Concatenation( "https://github.com/dlfer/symorb", LowercaseString( ~.PackageName ) ),
  

  ## anche qua, git di symorb
  SourceRepository := rec(
      Type := "git",
      URL := Concatenation( "https://github.com/dlfer/symorb", LowercaseString( ~.PackageName ) ),
  ),
   
  
  ArchiveURL := Concatenation( ~.SourceRepository.URL,
                                   "/releases/download/v", ~.Version,
                                   "/", ~.PackageName, "-", ~.Version ),


  ArchiveFormats := ".tar.gz",
  

  Persons := [
    rec( 
      LastName      := "Ferrario",
      FirstNames    := "Davide Luigi",
      IsAuthor      := true,
      IsMaintainer  := true,
      Email         := "davide.ferrario@unimib.it",
      WWWHome       := "http://www.matapp.unimib.it/~ferrario",
    ),
      
  ],
  

  Status := "dev",
  

  ## essendo collegato al git di symorb questi due file sono presenti nella repo
  README_URL := 
    Concatenation( ~.PackageWWWHome, "/README.md" ),
  PackageInfoURL := 
    Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
  
    
  ## qui dobbiamo inserire un abstract su symorb
  AbstractHTML := 
    "The <span class=\"pkgname\">Example</span> package, as its name suggests, \
     is an example of how to create a <span class=\"pkgname\">GAP</span> \
     package. It has little functionality except for being a package.",
  
  
  Dependencies := rec(
    
    ## il file di davide aveva un ">=4.3", non so se vogliamo dare piu compatibilita
    GAP := "4.10",
    NeededOtherPackages := [],
    SuggestedOtherPackages := [],
    ExternalConditions := []
                        
  ),
  
  
  AvailabilityTest := ReturnTrue,
  
  
  BannerString := Concatenation( 
       "----------------------------------------------------------------\n",
       "Loading  symorb", ~.Version, "\n",
       "by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
          " (", ~.Persons[1].WWWHome, ")\n",
       "For help, type: ?symorb package \n",
      "----------------------------------------------------------------\n" ),
  
  ));
