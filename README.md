# utl-ascii-art-outline-maps-of-states-and-countries-AI
Ascii art outline maps of states and countries

    Ascii art outline maps of states and countries

           Steps

              1, Choose a map dataset
                  C:\Program Files\sashome\SASFoundation\9.4\maps\africa.sas7bdat
              2, Use 'proc gmap' to produce outline map of africa (use fat outline woutline=3)
              3. Run R package 'imager' to conver outline to ascii characters


    github
    https://tinyurl.com/y5cr4fl5
    https://github.com/rogerjdeangelis/utl-ascii-art-outline-maps-of-states-and-countries-AI

    Danielle Navarro
    https://gist.github.com/djnavarro/793b2ebbec80c8819e7208dac1683d05

    Simon Barthelmé
    https://dahtah.github.io/imager/ascii_art.html

    *_                   _
    (_)_ __  _ __  _   _| |_
    | | '_ \| '_ \| | | | __|
    | | | | | |_) | |_| | |_
    |_|_| |_| .__/ \__,_|\__|
            |_|
    ;

    SAS 9.4M6 map dataset

     C:\Program Files\sashome\SASFoundation\9.4\maps\africa.sas7bdat

     MAPS.AFRICA total obs=52,824

       CONT     ID    SEGMENT    DENSITY        X          Y         LAT        LONG

        94     125       1          0       -0.45415    0.35555    0.47631     0.15126
        94     125       1          5       -0.43879    0.34538    0.46660     0.13428
        94     125       1          3       -0.42249    0.33434    0.45604     0.11629
        94     125       1          5       -0.40917    0.32516    0.44724     0.10161
        94     125       1          0       -0.39305    0.31380    0.43633     0.08387
        94     125       1          6       -0.38219    0.30602    0.42885     0.07196

    *            _               _
      ___  _   _| |_ _ __  _   _| |_
     / _ \| | | | __| '_ \| | | | __|
    | (_) | |_| | |_| |_) | |_| | |_
     \___/ \__,_|\__| .__/ \__,_|\__|
                    |_|
    ;

        Y |
          |
        0 +  1
          |                     aBhkLyqgprxN
          |                   fLU         pBZcp ovhH3wiTRt
          |                VboO               vDoh  ltGpFDT
          |              VYe                             YFn
          |              j                                8Bf
          |        uhu   D                                  S7
      -50 +        3xc  7n                                   XJC
          |             9Oy                                    az IJvy
          |               uX                                    kIN ja
          |                d0zerSlT4sQtl I                         cH
          |                   d        08OJ                      kt7
          |                         C  CU7q                    n0Z
          |                            0 75P                  Ul       VkF
     -100 +              j                 3g                JM        V V
          |                                 k                 12  LWz U   Y
          |                                nM                  n B8 yrv
          |                    q           z                EufRxfibRjQ  c     3
          |                                25              7X    iN tu   r47
          |                                 OZ            rVQ  I 6utsj
          |                                  m6          TLo      x
     -150 +                                   lin      ejY
          |              e                     5Vzvxi3aA
          |                B
          |                                                   l
          |  1                                                B
          |
          |
     -200 +
          |
          ---+--------------+--------------+--------------+--------------+----------
             0             50             100            150            200



     WANT total obs=1,246

      Obs      X     Y     VALUE     QV    CHAR

        1      1    -1    0.49024    55     v
        2    240    -1    0.49024    55     v
        3     92    -5    0.55761    60     l
        4     95    -5    0.41004    50     x
        5     96    -5    0.15773    31     X
        6     97    -5    0.21482    36     h
        7     98    -5    0.44212    51     L
        8     79    -6    0.39694    48     c
        9     80    -6    0.40471    49     y
       ....

    *          _       _   _
     ___  ___ | |_   _| |_(_) ___  _ __
    / __|/ _ \| | | | | __| |/ _ \| '_ \
    \__ \ (_) | | |_| | |_| | (_) | | | |
    |___/\___/|_|\__,_|\__|_|\___/|_| |_|

    ;

    * create jpeg map og africa;

    goptions reset=all;

    filename gout "d:/jpg/africa.jpg";

    goptions  device=jpeg gsfname=gout gsfmode=replace
              border htitle=12pt htext=10pt cback=white;

    data newaf;
       set maps.africa;
    region=1;
    run;

    /* Remove the unit areas from the AFRICA data set */
    proc gremove data=newaf out=africa;
      by region;
      id id;
    run;

    title;
    pattern value=mempty r=50 color=black ;
    proc gmap data=africa map=africa;
      id region;
      choro region / nolegend woutline=3;   * thicker gives more points important;
    run;quit;

    * digitize with ascii charaters;

    %utlfkil(d:/txt/r_pgm.txt);
    %utlfkil(d:/xpt/want.xpt);

    %utl_submit_r64('
    library(SASxport);
    library(tidyverse);
    library(imager);
    asciify <- function(file, charset, threshold){
      im <- load.image("d:/jpg/africa.jpg");
      im <- as.cimg(im[,,1:3]);
      greyval <- function(chr) {
        implot(imfill(50,50,val=1), text(25,25,chr,cex=5)) %>%
          grayscale %>%
          mean;
      };
      g <- map_dbl(charset, greyval);
      charset <- charset[order(g)];
      n <- length(charset);
      charmap <- grayscale(im) %>%
        imresize(.3) %>%
        as.data.frame %>%
        filter(value < threshold) %>%
        mutate(
          qv = cut_number(value, n) %>% as.integer,
          char = charset[qv]
        );
      return(charmap);
    };
    msg <- "ABCDEFG HIJKLMNOPQRSTUVWXYZ abcdefg hijklmnopqrstuvwxyz 1234567890";
    asc <- msg %>%
      str_squish() %>%
      str_split(pattern = "") %>%
      first() %>%
      unique();
    charsize <- 3;
    threshold <- .6;
    imgwidth <- 160;
    imgheight <- 110;
    charmap <- asciify(
      file = input,
      charset = asc,
      threshold = threshold);
    pic <- charmap %>%
      ggplot(aes(x, y)) +
      geom_text(aes(label = char), size = charsize) +
      scale_y_reverse() +
      theme_void();
    want<-as.data.frame(pic$data);
    str(want);
    write.xport(want,file="d:/xpt/want.xpt");
    ');

    * create SS table with points;
    libname xpt xport "d:/xpt/want.xpt";
    data want;
      set xpt.want;
      y=-y;
    run;quit;
    libname xpt clear;

    * may need to mess around with aspect?;
    options ls=100 ps=40;

    proc plot data=want;
      plot y*x=char;
    run;quit;

