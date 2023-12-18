\version "2.24.0"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 75))
  markup-system-spacing = 
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  last-bottom-spacing = 
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #158
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
global = {
  \key d \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \once \override Score.RehearsalMark.self-alignment-X = #LEFT
  \mark \markup\italic"To be sung in Unison."
  d4 e fis g |
  fis e \slurDashed d4( d) |
  a'4 a b cis |
  d2 d2 \bar "||"
  d4 e cis d |
  
  b a a fis |
  a g fis e |
  d( e) fis g |
  a g fis e |
  d4( d) d2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d1~|
  d4 cis d2 |
  d g~ |
  g4 fis8[ e] fis2 \bar "||"
  fis4 g e d |
  
  d2 cis4 d |
  d2 cis |
  b4 cis d2 |
  cis4 d cis2 |
  d4 b a2 \bar "|."
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  Pu -- er no -- bis ná -- sci -- tur
  Rec -- tor An -- ge -- ló -- rum,
  In hoc mun -- do pá -- sci -- tur
  Dó -- mi -- nus do -- mi -- nó -- rum,
  Dó -- mi -- nus do -- mi -- nó -- rum.
}
altoWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  In præ -- sé -- pe pó -- si -- tum
  Sub fæ -- no
  \unset ignoreMelismata
  a -- si -- nó -- rum
  Co -- gno -- vé -- runt Dó -- mi -- num
  Chris -- tum Re -- gem cæ -- ló -- rum,
  Chris -- tum Re -- gem cæ -- ló -- rum.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  Hunc He -- ró -- des tí -- mu -- it
  Ma -- gno cum tre -- mó -- re,
  In in -- fán -- tes ír -- ru -- it
  Hos cæ -- dens in fu -- ró -- re,
  Hos cæ -- dens in fu -- ró -- re.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  Qui na -- tus ex Ma -- rí -- a
  Di -- e ho -- di -- ér -- na
  Duc nos tu -- a grá -- ti -- a
  Ad gáu -- di -- a su -- pér -- na,
  Ad gáu -- di -- a su -- pér -- na.
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  Te Sal -- vá -- tor A et O
  Can -- té -- mus in cho -- ro,
  Can -- té -- mus in ór -- ga -- no,
  \set ignoreMelismata = ##t
  Be -- ne -- di -- cá -- mus Dó -- mi -- no,
  Be -- ne -- di -- cá -- mus Dó -- mi -- no.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  fis,4 g a b |
  a4. g8 fis2 |
  a4 d~ d g, |
  a1 \bar "||"
  b4~b a~ a |
  
  g fis e d |
  a' b a g |
  fis2. d4 |
  e b' a g |
  fis g fis2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d2. g,4 |
  a2 d |
  fis g4 e |
  d1 \bar "||"
  b'4 e, a fis |
  
  g d a b |
  fis g a2 |
  b2.~ b4 |
  a1 |
  d2 d, \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
%6.14 \context {\Lyrics\override LyricText.font-size = #0.9 }
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Puer nobis nascitur"}}
    poet = \markup\concat{\oldStyleNum"Words and tune (14th cent.) from " \italic"Piæ Cantiones" \oldStyleNum", 1582"}
    composer = \markup\oldStyleNum"Arranged by G.H. Palmer"
    tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
  }
}












global = {
  \key d \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  d4 e fis g |
  fis e \slurDashed d4( d) |
  a'4 a b cis |
  d2 d4( d) \bar "||"
  d4 e cis d |
  
  b a a fis |
  a g fis e |
  d( e) fis g |
  a g fis e |
  d4( d) d2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d1~|
  d4 cis d2 |
  d g~ |
  g4 fis8[ e] fis2 \bar "||"
  fis4 g e d |
  
  d2 cis4 d |
  d2 cis |
  b4 cis d2 |
  cis4 d cis2 |
  d4 b a2 \bar "|."
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Un -- to us is born a Son, _
  King of Quires su -- per -- nal: _
  \unset ignoreMelismata
  See on earth His life be -- gun,
  Of lords the Lord e -- ter -- nal,
  Of lords the Lord e -- ter -- nal,
}
altoWordsII = \lyricmode {
  \set stanza = #"2. "
  Christ, from heav’n de -- scend -- ing low,
  Comes on earth a stran -- ger:
  Ox and ass their Own -- er know
  Be -- cra -- dled in the man -- ger,
  Be -- cra -- dled in the man -- ger.

}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  This did Her -- od sore af -- fray,
  \unset ignoreMelismata
  And griev -- ous -- ly be -- wil -- der;
  So he gave the word to slay,
  And slew the lit -- tle chil -- der,
  And slew the lit -- tle chil -- der.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  Of His love and mer -- cy mild
  \set ignoreMelismata = ##t
  This the Christ -- mas sto -- ry:
  \unset ignoreMelismata
  And O that Ma -- ry’s gen -- tle Child
  Might lead us up to glo -- ry,
  Might lead us up to glo -- ry!
  
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  \markup\italic O \markup\italic et \markup\italic A \markup\italic et \markup\italic A \markup\italic et \markup\italic O,
  \markup\italic Cum \markup\italic cán -- \markup\italic ti -- \markup\italic bus \markup\italic in \markup\italic cho -- \markup\italic ro,
  \markup\italic Cum \markup\italic cán -- \markup\italic ti -- \markup\italic cis \markup\italic et \markup\italic ór -- \markup\italic ga -- \markup\italic no,
  \markup\italic Be -- \markup\italic ne -- \markup\italic di -- \markup\italic cá -- \markup\italic mus \markup\italic Dó -- \markup\italic mi -- \markup\italic no.
  \markup\italic Be -- \markup\italic ne -- \markup\italic di -- \markup\italic cá -- \markup\italic mus \markup\italic Dó -- \markup\italic mi -- \markup\italic no.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  fis,4 g a b |
  a4. g8 fis2 |
  a4 d~ d g, |
  a1 \bar "||"
  b4~b a~ a |
  
  g fis e d |
  a' b a g |
  fis2. d4 |
  e b' a g |
  fis g fis2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d2. g,4 |
  a2 d |
  fis g4 e |
  d1 \bar "||"
  b'4 e, a fis |
  
  g d a b |
  fis g a2 |
  b2.~ b4 |
  a1 |
  d2 d, \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
%6.14 \context {\Lyrics\override LyricText.font-size = #0.9 }
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
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
    %title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Unto us is born a Son"}}
    subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(The same, in English)"}}
    %poet = \markup{\oldStyleNum"Words and tune (14th cent.) from" \italic"Piæ Cantiones," 1582}
    %composer = \markup\oldStyleNum"Arranged by G.H. Palmer"
  }
}
\header{    tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}}

