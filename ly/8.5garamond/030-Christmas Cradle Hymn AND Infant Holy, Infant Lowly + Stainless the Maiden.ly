\version "2.14.2"
\include "util.ly"
\header {
  tagline = ""
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 2)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 50))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  % last-bottom-spacing =
  %   #'((basic-distance . 0)
  %      (minimum-distance . 0)
  %      (padding . 0)
  %      (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #30
  print-first-page-number = ##t
  scoreTitleMarkup = \markup { \column {
    %\on-the-fly \print-all-headers { \bookTitleMarkup \hspace #1 }
    \fill-line {
      \null
      \fontsize #4 \bold \fromproperty #'header:title
      \null
    }
    \fill-line {
      \fromproperty #'header:poet
      \fontsize #4 \bold \fromproperty #'header:subtitle
      \fromproperty #'header:composer
    }
    \fill-line {
      \fromproperty #'header:meter
      \null
      \fromproperty #'header:arranger
    }
  }
  }

  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key ees \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
  g'4 g8[ f] g4 |
  aes aes g |
  f f g8[ aes] |
  c4( bes2) |
  g4 g8[ f] g4 |
  
  aes aes g |
  f bes4. aes8 |
  aes4( g2) |
  \repeat volta 2 {
    bes4 bes ees8[ d] |
    c4 c8[ bes] c[ bes] |

    aes4 aes c |
    \slurDashed c( bes2) |
    g4 g8[ f] g4 |
    aes aes g |
    f bes4. aes8 |
    aes4( g2)
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 ees ees |
  ees ees ees |
  d d d |
  ees~ ees2 |

  ees4 ees ees |
  ees ees ees |
  d d4. f8 |
  ees4~ ees2 |

  \repeat volta 2 {
    ees4 ees bes' |
    aes ees ees |
    d8[ ees] f[ ees] aes4 |
    \slurDashed aes( g2) |

    ees4 ees ees |
    ees ees ees |
    d d4. f8 |
    \tieDashed ees4~ ees2 |
  }
}
altoWords = \lyricmode {
  \set stanza = "1. "

  Hush Thee, my lit -- tle One,
  Moth -- er is nigh;
  At Thy low man -- ger bed
  Watch -- ing close by. __
  O -- ver the mead -- ows dim
\set ignoreMelismata = ##t
   \once \override LyricText.self-alignment-X = #LEFT Night
   \once \override LyricText.self-alignment-X = #LEFT winds 
   \once \override LyricText.self-alignment-X = #LEFT are blow -- ing,
\unset ignoreMelismata
  Here in the qui -- et byre,
\set ignoreMelismata = ##t
  Meek cat -- tle low -- ing.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = "2. "

  Look lit -- tle wake -- ful One
  Out at the sky, __
  Where stars like sil -- ver lamps
  Swing far on high.
  Dost know, Child, how three kings
  \once \override LyricText.self-alignment-X = #LEFT Rid --
  \once \override LyricText.self-alignment-X = #LEFT ing
  \once \override LyricText.self-alignment-X = #LEFT from far __
  Brought to Thy crib rich gifts,
  Led by Thy star.
}
altoWordsIII = \lyricmode {
  \set stanza = "3. "

  Dost know how, on the night
  When Thou wert born,
  An -- gels sang songs of joy
  From dark till dawn.
  But, now, my Ba -- by, rest,
  \once \override LyricText.self-alignment-X = #LEFT While
  \once \override LyricText.self-alignment-X = #LEFT I
  \once \override LyricText.self-alignment-X = #LEFT am nigh,
  Sleep, drow -- sy lit -- tle Boy,
  Hush, hush -- a -- by. __
}
altoWordsIV = \lyricmode {
  \set stanza = "4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = "5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = "6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  bes4 bes b |
  c c bes?8[ ees] |
  d[ c] bes[ aes] g[ f] |
  aes4( g2) |

  bes4 bes ees8[ des] |
  c4 c bes8[ ees] |
  d?[ c] bes[ c] d4 |
  c( bes2) |

  \repeat volta 2 {
    g4 g8[ f] g4 |
    aes aes8[ g] aes[ g] |
    f[ ees] d[ c] d[ ees] |
    \slurDashed aes4( g2) |

    bes4 bes ees8[ des] |
    c4 c bes |
    d?8[ c] bes[ c] d4 |
    c( bes2) |
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 ees ees |
  aes aes ees |
  bes bes bes |
  ees~ ees2 |

  ees4 ees ees |
  aes aes ees |
  bes bes4. bes8 |
  ees4~ ees2 |

  \repeat volta 2 {
    ees4 ees ees |
    ees ees ees |
    bes bes aes |
    \tieDashed ees'~ ees2 |
    
    ees4 ees ees |
    aes aes ees |
    bes bes4. bes8 |
    ees4~ ees2 |
  }
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWords
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #2
  }
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Christmas Cradle Hymn"}}
    subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Lulajże, Jezuniu)"}}
    poet = \markup\oldStyleNum"Traditional Polish Carol"
    meter = \markup\oldStyleNum"Adapted to English by Mary Strawn Vernon"
    composer = \markup\oldStyleNum"Traditional Polish Carol"
    % arranger = \markup\oldStyleNum"Arranged by Edith M. G. Reed (1885–1933)"
    tagline = ""
  }
}

























global = {
  \key g \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
\repeat unfold 2 {
  \partial 4 d8 d |
  g4 g fis8 g |
  a4 a b8 c |
  d4 c b8  a |
  g2 \break
}
\pageBreak
\repeat volta 2 {
  \partial 4 g8 fis |
  e4 e a8 g |
  fis4 fis b8 a |
  \partial 2 g4 g | \break

  \partial 4 c8 b |
  a4 a b8 c |
  d4 c b8 a |
  g2 
}
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
\repeat unfold 2 {
  d8 d |
  d4 d d8 e |
  fis4 fis g8 g |
  g4 g fis8 fis |
  d2
}

\repeat volta 2 {
    d8 d |
    c4 c c8 c |
    d4 d d8 d
    e4 e

    e8 e |
    fis4 fis e8 e |
    g4 g fis8 fis |
    d2 
  }
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = "1. "
  In -- fant ho -- ly, in -- fant low -- ly
  For His bed a cat -- tle stall;
  Ox -- en low -- ing, lit -- tle know -- ing,
  Christ the Babe, is Lord of all.
  Swift are wing -- ing an -- gels sing -- ing,
  No -- ëls ring -- ing,
  tid -- ings bring -- ing:
  Christ the Babe is Lord of all.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = "2. "
Flocks were sleep -- ing, shep -- herds keep -- ing
Vi -- gil till the morn -- ing new
Saw the glo -- ry, heard the sto -- ry,
Tid -- ings of a gos -- pel true.
Thus re -- joic -- ing, free from sor -- row,
Prais -- es voic -- ing greet the mor -- row:
Christ the Babe was born for all.
}
altoWordsIII = \lyricmode {
  \set stanza = "3. "
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \set stanza = "4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = "5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = "6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c {
  \repeat unfold 2{
    d8 d |
    b'4 b d,8 d |
    d'4 d d8 c |
    d4 e d8 c |
    b2
  }
  \repeat volta 2 {
    g8 g |
    g4 g a8 a |
    a4 a b8 b 
    b4 b a8 a |
    a4 a g8 g8 |
    d'4 e d8 c |
    b2 
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat unfold 2 {
    d8 d |
    g4 g d8 d |
    d'4 d g,8 e |
    b4 c d8 d |
    g2
  }

  \repeat volta 2 {
    b,8 b |
    c4 c a8 a |
    d4 d b8 b
    e4 e

    a,8 a |
    d4 d e8 c |
    b4 c d8 d |
    g,2
  }
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Infant Holy, Infant Lowly"}}
    subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(W Żłobie Leży)"}}
    poet = \markup\oldStyleNum"Traditional Polish Carol"
    meter = \markup\oldStyleNum"Translated by Edith M. G. Reed (1885–1933)"
    composer = \markup\oldStyleNum"Traditional Polish Carol"
    arranger = \markup\oldStyleNum"Arranged by Edith M. G. Reed (1885–1933)"
    tagline = \markup { "from" \italic "CyberHymnal.org"}
  }
}

























global = {
  \key d \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  a'2 a4 g |
  fis2 fis |
  fis4 fis e fis |
  a2 g |
  g g4 fis |
  e2 e |
  b'4 b a g |
  g2 fis |

  \repeat volta 2 {
    a2 a4 a |
    b2 b |
    cis4 cis b cis |
    d2 a |
    d cis4 b |
    b2 a |
    b4 a g cis, |
    e2 d
  }
}
sopWords = \lyricmode {
}

altoMusic = \relative c' {
  fis2 fis4 e |
  d2 d |
  d4 d cis d |
  fis2 e |
  e e4 d |
  cis2 cis |
  g'4 g fis e |
  e2 d |

  \repeat volta 2 {
    fis fis4 fis |
    g2 g |
    g4 g g g |
    fis2 fis |
    b a4 g |
    g2 fis |
    g4 fis e cis |
    cis2 d
  }
}
altoWords = \lyricmode {
  \set stanza = "1. "

  Stain -- less the Maid -- en
  Whom He chose for moth -- er;
  Nine months she wait -- ed,
  Bear -- ing Christ, our broth -- er;
  Think of her glad -- ness
  When at last she saw Him:
  God in a man -- ger,
  Beth -- le -- hem a heav -- en!
}
altoWordsII = \lyricmode {
  \set stanza = "2. "

  Lan -- tern in dark -- ness,
  When the sick are sigh -- ing,
  Thresh -- old of bright -- ness,
  Com -- fort of the dy -- ing,
  High she is hold -- ing
  For a world a -- dor -- ing,
  Hope of the na -- tions,
  Je -- sus Christ, our broth -- er.
}
altoWordsIII = \lyricmode {
  \set stanza = "3. "
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \set stanza = "4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = "5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = "6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a2 a4 a |
  a2 a |
  a4 a a a |
  b2 b |
  a a4 a |
  a2 a |
  a4 a a a |
  a2 a |

  \repeat volta 2 {
    d2 d4 d |
    d2 d |
    e4 e e e |
    d2 d |
    d2 d4 d |
    d2 a |
    b4 b b g |
    g2 fis
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d2 d4 e |
  fis2 d |
  fis4 fis g fis |
  dis2 e |
  cis cis4 d |
  e2 a, |
  cis4 cis d e |
  a,2 d |

  \repeat volta 2 {
    d' d4 cis |
    b2 b |
    a4 a a ais |
    b2 fis |
    g g4 g |
    d2 d |
    dis4 dis e e |
    a,2 d
  }
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Stainless the Maiden"}}
    subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Serdeczna Matko)"}}
    poet = \markup\oldStyleNum"Traditional Polish Hymn"
    %meter = \markup\oldStyleNum"Translated by ??? (1885–1933)"
    composer = \markup\oldStyleNum"Traditional Polish Hymn"
    %arranger = \markup\oldStyleNum"Arranged by ??? (1885–1933)"
    tagline = ""
  }
}
