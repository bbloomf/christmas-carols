\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Fum, Fum, Fum"}}
  poet = \markup\oldStyleNum"Catalonian"
  composer = \markup\oldStyleNum"Arranged by Abel Di Marco, Pbro."
  tagline = \markup { "from" \italic "cpdl.org" "and" \italic"pucpr.edu"}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #176
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
  \key c \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \slurDashed
  \tempo 4 = 88
  a'8. gis16 a8 c b a |
  gis( e) a b8\rest gis b\rest
  \time 2/4 a2 \bar "||" \break
  
  \time 3/4 a8. gis16 a8 c b a |
  gis( e) a b8\rest gis b\rest
  \time 2/4 a4. a16 b | \break
  \repeat volta 2 {
    c8 c b b |
    
    c c b b |
    c c b b |
    c4 b8\rest b16 c |
    d8. c16 b8 a |
    gis e a gis |
    
    %page2
    a8 c b a |
    \time 3/4 gis8^\markup\italic"rall. before 2nd ending" e a b\rest gis b\rest |
  }
  \alternative {
    {
      \time 2/4 a4. a16 b
    }
    {
      \time 2/4 a2 \bar "|."
    }
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \slurDashed
  e8. e16 e8 e f f |
  e( e) e s e s |
  e2 |
  
  e8. e16 e8 e f f |
  e( e) e s e s |
  e4. a16 gis |
  a8 a gis gis |
  
  a a f f |
  g g f f |
  g4 s8 gis16 a |
  b8. a16 gis8 f |
  e e e e |
  
  %page2
  e e f f |
  e e e s e s |
  e4. a16 gis |
  
  e2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set ignoreMelismata = ##t
  \set stanza = #"1. "
  Twen -- ty -- fifth day of De -- cem -- ber,
  \markup\italic Fum, \markup\italic fum, \markup\italic fum!
  Twen -- ty -- fifth day of De -- cem -- ber,
  \markup\italic Fum, \markup\italic fum, \markup\italic fum!
  
  For a bless -- ed Babe was born
  Up -- on this day at break of morn
  In a man -- ger poor and low -- ly
  Lay the Son of God most ho -- ly
  \markup\italic Fum, \markup\italic fum, \markup\italic fum!
  
  For a
  \markup\italic fum!
  
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set ignoreMelismata = ##t
%\markup\italic
  \set stanza = #"2. "
  Praise we now the Lord a -- bove, __ _
  \markup\italic Fum, \markup\italic fum, \markup\italic fum!
  Praise we now the Lord a -- bove, __ _
  \markup\italic Fum, \markup\italic fum, \markup\italic fum!
  Now we all our voi -- ces raise
  And sing a song of grate -- ful praise
  Cel -- e -- brate in song and sto -- ry
  All the won -- ders of His glo -- ry
  \markup\italic Fum, \markup\italic fum, \markup\italic fum!
  
  Now we
  \markup\italic fum!
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \slurDashed
  c8. b16 c8 e d c |
  b( gis) a r b r |
  c2 |
  
  c8. b16 c8 e d c |
  b( gis) a r b r |
  c4. c16 d |
  e8 e d d |
  
  e e d d |
  e e d d |
  e4 s8 d16 e |
  f8. e16 d8 c |
  b gis c b |
  
  %page2
  c8 e d c |
  b gis a r b r |
  c4. c16 d |
  
  c2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \slurDashed
  a8 e' a a d, d |
  e( d) c d e e |
  a e a,4 |
  
  a8 e' a a d, d |
  e( d) c d e e |
  a e a,4 |
  a e' |
  
  a d,\rest |
  g g c, d8\rest e16 e |
  d8 e e d |
  e e a e |
  
  %page2
  a8 a d, d |
  e d c d e e |
  a e a,4 |
  
  a'8 e a,4 \bar "|."
}
bassWords = \lyricmode {
  \repeat unfold 7 \skip1
  \markup\italic Fum, \markup\italic fum, \markup\italic fum, \markup\italic fum, 
  \markup\italic fum, \markup\italic fum, \markup\italic fum.
  
  \repeat unfold 7 \skip1
  \markup\italic Fum, \markup\italic fum, \markup\italic fum, \markup\italic fum, 
  \markup\italic fum, \markup\italic fum, \markup\italic fum.
  
  \markup\italic Fum, \markup\italic fum, \markup\italic fum,
  \markup\italic fum, \markup\italic fum, \markup\italic fum.
  
  \repeat unfold 16 \skip1
  \markup\italic Fum, \markup\italic fum, \markup\italic fum, \markup\italic fum, 
  \markup\italic fum, \markup\italic fum, \markup\italic fum.
  
  
  \markup\italic fum, \markup\italic fum, \markup\italic fum.
  
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 0) (padding . -0.5)) } \lyricsto "basses" \bassWords
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
}

