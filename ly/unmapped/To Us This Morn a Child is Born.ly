\version "2.14.2"
\include "util.ly"
\header {
  tagline = \markup \concat{ "from " \italic "The Cambridge Carol Book" \oldStyleNum", 1924"}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"To Us This Morn a Child is Born"}}
    poet = \markup\oldStyleNum"George Ratcliffe Woodward (1848–1934)"
    composer = \markup{\italic "Jog on, jog on the footpath way"}
    arranger = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -20)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #160
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key g \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 8 d8 |
  g4 d8 g4 a8 |
  b8.[ c16] b8 a[ fis] d |
  g8.[ a16] g8 d'4 c8 |
  b4. a4 \bar""\break d,8 |
  
  a'4 b8 c8.[ d16] c8 |
  b8.[ c16] b8 a8.[ g16] fis8 |
  g8.[ a16] g8 fis8.[ g16] fis8 |
  \partial 8*5 e4. d4 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  d4 d8 e4 fis8 |
  g4 g8 fis[ d] d |
  e4 g8 g4 g8 |
  g4. fis4 d8 |
  
  d4 g8 g4 fis8 |
  g4 g8 fis8.[ e16] d8 |
  b4 cis8 d4 d8~ |
  d cis!4 d4 \bar "|."
}
altoWords = \lyricmode {  
  \dropLyricsIX
  \set stanza = #"1. "
  To us this morn a Child is born,
  His Fa -- ther is none o -- ther
  Than God the King of ev -- ’ry thing,
  Maid Ma -- ry is His Mo -- ther.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Her Babe is Lord by all a -- dored
  I -- sa -- iah had fore -- shown her:
  Now came’t to pass that ox and ass
  Bow’d down a -- fore their Own -- er.
  \set ignoreMelismata = ##t
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  When Her -- od heard the Ma -- ges’ word,
  He smote the babes a -- sun -- der
  In all that coast, a blame -- less host,
  From two years old and un -- der.
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Now, faith -- ful quire, bless God the Sire,
  Bless God the Spi -- rit Ho -- ly,
  Bless God, the Son ere time be -- gun,
  Now lain in man -- ger low -- ly.
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
  b8 |
  b4 a8 b4 d8 |
  d8.[ e16] d8 d4 b8 |
  b4 c8 d4 e8 |
  d4. d4 b8 |
  
  a4 d8 c4 a8 |
  d4 d8 d4 a8 |
  g4 g8 a4 a8 |
  a4. fis4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'8 |
  g4 fis8 e4 d8 |
  g4 g,8 d'4 g8 |
  e4 e8 b4 c8 |
  g'4. d4 g8 |
  
  fis4 g8 e4 a8 |
  g4 g8 d4 d8 |
  e4 e8 fis4 d8 |
  a4. d4 \bar "|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "basses" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
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
  
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
