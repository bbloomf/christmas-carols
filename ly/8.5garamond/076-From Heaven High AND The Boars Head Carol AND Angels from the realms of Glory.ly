\version "2.24.0"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
%{IF_LESSER
  markup-system-spacing.stretchability = 50
  top-markup-spacing.stretchability = 30
  last-bottom-spacing.stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #076
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}

#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
%IF_NOT_LESSER
global = {
  \key c \major
  \time 4/4
  \autoBeamOff
  \mergeDifferentlyHeadedOn
}

sopMusic = \relative c'' {
  \tempo \markup\italic"Very slow and dignified" 4 = 46
  \partial 4 c4 |
  b a b g |
  a b c\fermata \bar "||"
  
  c |
  c g g e8[ f] |
  g4 f e\fermata \bar"||"
  
  e |
  a a g8[ a] b4 |
  c8[ b] a4 g\fermata
  
  c4 |
  b a g a8[ g] |
  f[ e] d4 c\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  g'4 |
  g8[ fis] e4 d e8[ d] |
  c4 d e |
  
  e8[ f] |
  g[ f] e4 d c |
  c d c |
  
  c8[ d] |
  e4 d d8[ fis] g4 |
  g g8[ fis] g4 |
  
  e8[ d] |
  d[ e] e[ d] d[ c] c4 |
  c8[ a] b4 c \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  From heav -- en high I come to you,
  To bring you ti -- dings, strange and true.
  Glad ti -- dings of great joy I bring,
  Where of I now will say and sing.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  To you this night is born a Child
  Of Ma -- ry, chos -- en Moth -- er mild;
  This lit -- tle Child, of low -- ly birth,
  Shall be the joy of all the earth.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Glo -- ry to God in high -- est Heav’n,
  Who un -- to us His Son hath giv’n!
  While an -- gels sing with pi -- ous mirth
  A glad New Year to all the earth.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  e4 |
  d c b c8[ b] |
  a[ g] f4 g |
  
  g4 |
  c c b bes8[ a] |
  g4 g g |
  
  g |
  g fis g8[ c] b[ a] |
  g[ b] e[ d] b4 |
  
  a4 |
  g g8[ f] f[ e] a[ b] |
  c4 g8[ f] e4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c4 |
  g'4. fis8 g[ f] e4 |
  f8[ e] d4 c |
  
  c8[ d] |
  e4 e8[ f] g4 g8[ f] |
  e[ d] c[ b] c4 |
  
  c |
  cis d8[ c] b[ a] g4 |
  e'8[ d] c[ d] g4 |
  
  g8[ fis] |
  g[ e] c[ d] b[ c] f[ g] |
  a[ f] g[ g,] c4 \bar "|."
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"From Heaven High I Come to You"}}
    poet = \markup\oldStyleNum"Martin Luther (1483–1546)"
    meter = \markup\oldStyleNum"Translated by Catherine Winkworth (1827–1878)"
    composer = \markup\oldStyleNum"Old German Melody Attributed to Martin Luther"
    arranger = \markup\oldStyleNum"Adapted by J.S. Bach (1685–1750)"
    tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}
  }
}
\markup\fill-line{\concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}}
\markup\vspace#2

%END_IF_NOT_LESSER

















%Boars Head Carol
global = {
  \key c \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
  \mergeDifferentlyDottedOn
}

sopMusic = \relative c'' {
  \partial 4 g4 |
  \slurDashed c4( c) c4( c8) c8 |
  b4 c g4.*2/3 \teeny e8\noBeam \normalsize e |
  
  f4 f a4. f8 |
  g4. g8 c4 \bar""\break g8\noBeam( g) |
  c4 c8\noBeam( c) c4 c |
  
  b c g4. e8 |
  f4 f a4. f8 |
  g4. g8 c2 | \break
  
  \repeat volta 2 {
    c4. c8 b4 b |
    c c g2 |
    f4 f a4. f8 |
    g4. g8 c4\fermata
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 |
  \slurDashed e( e) e4( e8) e8 |
  g4 g e4.*2/3 \teeny e8\noBeam \normalsize e |
  
  f4 f f4. c8 |
  \slurSolid f8[ e] d4 e \slurDashed e8\noBeam( e) |
  e4 e8\noBeam( e) e4 e |
  
  g g e4. e8 |
  f4 f f4. c8 |
  \slurSolid f[ e] d4 e2 |
  
  e4. e8 d4 d |
  e e d2 |
  c4 c c4. d8 |
  \slurSolid d[ e] f4 e |
}
altoWords = {
  \dropLyricsIX
  \lyricmode {
    \set stanza = #"1."
    \set ignoreMelismata = ##t
    The boar’s _ head _ in hand bear I "" Be -- decked with bays and rose -- ma -- ry;
    \set associatedVoice = "basses"
    And I pray you my mas -- ters mer -- ry be;
    \markup\italic Quot _ \markup\italic es -- \markup\italic tis _ \markup\italic in \markup\italic con -- \markup\italic ví -- \markup\italic vi -- \markup\italic o.
  }
  \set stanza = \markup\dynamic"  ff - pp"
  \lyricmode {
    \unset ignoreMelismata
    \markup\italic Ca -- \markup\italic put \markup\italic a -- \markup\italic pri \markup\italic dé -- \markup\italic fe -- \markup\italic ro
    \set associatedVoice = "tenors"
    \markup\italic Red -- \markup\italic dens
    \set associatedVoice = "basses"
    \markup\italic lau -- \markup\italic des \markup\italic Dó -- \markup\italic mi -- \markup\italic no.
  }
}
altoWordsII = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic"f " "2."}
  \lyricmode {
  %\markup\italic
    \set ignoreMelismata = ##t
    The boar’s _ head as I un -- der -- stand Is the rar -- est dish in all this land,
    \set associatedVoice = "basses"
    Which is thus be -- _ decked with_a gay gar -- land,
    Let _ us
    \markup\italic ser -- _ \markup\italic ví -- \markup\italic re \markup\italic cán -- \markup\italic ti -- \markup\italic co.
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3."
  \set ignoreMelismata = ##t
  Our stew -- ard hath _ pro -- vid -- ed this
  "" In hon -- or of the King of bliss,
  \set associatedVoice = "basses"
  Which _ on this __ _ day to_be serv -- ed is,
  \markup\italic In _ \markup\italic Re -- \markup\italic gi -- _ \markup\italic nén -- \markup\italic si \markup\italic á -- \markup\italic tri -- \markup\italic o.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  c4 |
  \slurDashed g( g) g4( g8) g8 |
  d'4 e c4.*2/3 \teeny c8\noBeam \normalsize c |
  
  c4 c c4. a8 |
  c4 b c c8\noBeam( c) |
  g4 g8\noBeam( g) g4 g |
  
  d' e c4. c8 |
  c4 c c4. a8 |
  c4 b c2 |
  
  g4. g8 g4 g |
  g a \slurSolid c( b) |
  a a a4. a8 |
  c4 b c |
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c4 |
  \slurDashed c( c) c4( c8) c8 |
  g'4 c, c4 c'8\noBeam( bes) |
  
  a4 \slurSolid a8[ g] f4. f8 |
  g4 g c, \slurDashed c8\noBeam( c) |
  c4 c8\noBeam( c) c4 c |
  
  g' c, c'4 c8[ bes] |
  a4 \slurSolid a8[ g] f4 f |
  g4 g c,2 |
  
  c4. c8 g'4 g |
  c, a' g2 |
  a4 a8[ g] f[ e] d4 |
  g4 g, c\fermata |
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "tenors" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Boar’s Head Carol"}}
    poet = \markup\oldStyleNum"15th Century English"
    composer = \markup\oldStyleNum"Traditional English"
    tagline = ""
  }
}
%IF_NOT_LESSER
\markup\vspace#8
%END_IF_NOT_LESSER





















%Angels From the Realms of Glory
global = {
  \key c \major
  \time 4/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
  \autoBeamOff
}

sopMusic = \relative c'' {
  \tempo 4 = 104
  g4 e c' g |
  e'4. d8 c4 g |
  a a g c |
  g f e2 | \break
  
  g4 e c' g |
  e'4. d8 c4 b |
  c b a b8[ c] |
  b4 a g2 | \break
  
  d'4. d8 b4 g |  
  e'4. d8 c4 a |
  f' e d c |
  c b c2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 c g' e |
  g4. g8 g4 g |
  c, c c c |
  
  d b c2 |
  e4 c g' g8[ f] |
  e4. f8 e4 e |
  
  e e e e |
  g fis g2 |
  g4. g8 g4 g |
  
  g4. e8 f4 f |
  a g f e8[ f] |
  g4. f8 e2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  An -- gels, from the realms of glo -- ry,
  Wing your flight o’er all the earth,
  \set associatedVoice = "basses"
  Ye, who sang cre -- a -- tion’s sto -- ry,
  \unset associatedVoice
  Now pro -- claim Mes -- si -- ah’s birth;
  Come and wor -- ship, come and wor -- ship,
  \set associatedVoice = "basses"
  Wor -- ship
  \set associatedVoice = "altos"
  Christ, the
  \unset associatedVoice
  new -- born King.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Shep -- herds, in the field a -- bid -- ing, Watch -- ing o’er your flocks by night,
  \set associatedVoice = "basses"
  God with man is now re -- sid -- ing;
  \unset associatedVoice
  Yon -- der shines the in -- fant light;
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Sa -- ges, leave your con -- tem -- pla -- tions, Bright -- er vis -- ions beam a -- far;
  \set associatedVoice = "basses"
  Seek the great De -- sire of na -- tions,
  \unset associatedVoice
  Ye have seen His na -- tal star;
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Saints be -- fore the al -- tar bend -- ing,
    Watch -- ing long in hope and fear,
  \set associatedVoice = "basses"
  Sud -- den -- ly the Lord, de -- scend -- ing, 
  \unset associatedVoice
    In His tem -- ple shall ap -- pear;
}
tenorMusic = \relative c' {
  c4 g g c |
  c4. b8 c4 c |
  a c g a |
  
  g g g2 |
  c4 c g g |
  c4. b8 a4 gis |
  
  a gis e' d8[ c] |
  d4 c b2 |
  b4. b8 d4 b |
  
  c4. bes8 a4 c |
  d g, a8[ b] c4 |
  d4 d c2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c4 c e c |
  g'4. f8 e4 e |
  f f e a, |
  
  b g c2 |
  c'4 g e e8[ d] |
  c4. d8 e4 e |
  
  a e c a |
  d d g,2 |
  g'4. g8 g4 g |
  
  c,4. c8 f4 f |
  d e f8[ g] a4 |
  g g, c2 \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \context Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \context Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \context Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Angels, from the Realms of Glory"}}
    poet = \markup\oldStyleNum"James Montgomery (1771–1854)"
    composer = \markup\oldStyleNum"Henry Smart (1813–1879)"
    tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
  }
}
\header {
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
}

