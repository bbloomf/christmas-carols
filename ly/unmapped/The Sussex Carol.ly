\version "2.14.2"
\include "util.ly"
\version "2.14.2"
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Sussex Carol"}}
  poet = \markup\oldStyleNum"Traditional English (17th century or earlier)"
  %composer = "Arranged by BHB"
  tagline = ""
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #098
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
global = {
  \key f \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
  \override Score.TimeSignature #'break-visibility = #'#(#f #f #f)
  \slurDotted
}

sopMusic = \relative c'' {
  \repeat volta 2 {
    \partial 8 c8 |
    c4 a8 bes4 c8 |
    a( g) f g4 e8 |
    f4 f8 g[ a] bes |
    
  }
  \alternative {
    { \partial 8*5 a4 g8 f4 }
    { a4 g8 f4. }
  }
  
  g4. g4 f8 |
  g[ a] bes c[ bes] a |
  
  g2. |
  \time 9/8 c4. d c |
  \time 6/8 bes4 a8 g[ f] g |
  f2. \bar "|." \break
  
  
  
  
  \repeat volta 2 {
    \partial 8 c'8 |
    c4 a8 bes4 c8 |
    a[ g] f g4 e8 |
    f4 f8 g[ a] bes |
    
  }
  \alternative {
    { \partial 8*5 a4 g8 f4 }
    { a4 g8 f4. }
  }
  
  g4. g4 f8 |
  g[ a] bes c[ bes] a |
  
  g2. |
  \time 9/8 c4. d c |
  \time 6/8 bes4 a8 g[ f] g |
  f2. \bar "|." \break
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  \repeat volta 2 {
    a8 |
    a4 f8 f4 e8 |
    f( f) f8 c4 c8 |
    d4 d8 c[ f] e |
  }
  \alternative {
    { f4 c8 f4 | }
      
    { f4 c8 f4. | }
  }
  e d4 d8 |
  e[ f] g a[ g] f |
  
  e2. |
  f4. f f |
  f4 f8 c[ d] e |
  f2. \bar "|."
  
  
  
  
  \repeat volta 2 {
    a8 |
    a4 f8 f4 e8 |
    f4 f8 c4 c8 |
    d4 d8 c[ f] e |
  }
  \alternative {
    { f4 c8 f4 | }
  
    { f4 c8 f4. | }
  }
  e d4 d8 |
  e[ f] g a[ g] f |
  
  e2. |
  f4. f f |
  f4 f8 c[ d] e |
  f2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  On Christ -- mas night all Chris -- tians sing
  To hear the news the an -- gels bring.
  an -- gels bring:
  
  News of great joy, news of great mirth,
  News of our mer -- ci -- ful King’s birth.
  
  
  \set stanza = #"3. "
  When sin de -- parts be -- fore His grace,
  Then life and health come in its place,
  in its place.
  
  An -- gels and men with joy may sing,
  All for to see the new -- born King.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Then why should men on earth be so sad,
  \unset ignoreMelismata
  Since our Re -- deem -- er made us glad?
  made us glad?
  
  When from our sin He set us free,
  All for to gain our lib -- er -- ty.
  
  \set stanza = #"4. "
  All out of dark -- ness we have light,
  Which made the an -- gels sing this night,
  sing this night:
  
  “Glo -- ry to God and peace to men,
  Now and for ev -- er -- more, A -- men.”
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
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
  \repeat volta 2 {
    c8 |
    c4 c8 d4 c8 |
    c( bes) a bes4 bes8 |
    a4 a8 bes[ a] c |
  }
  \alternative {
    { d4 bes8 a4 | }
  
    { d4 bes8 a4. | }
  }
  g bes4 a8 |
  bes[ f] c' f,4 c'8 |
  
  c2. |
  a4. bes a |
  d4 c8 c[ a] bes |
  a2. \bar "|."
  
  
  
  
  \repeat volta 2 {
    c8 |
    c4 c8 d4 c8 |
    c[ bes] a bes4 bes8 |
    a4 a8 bes[ a] c |
  }
  \alternative {
    { d4 bes8 a4 | }
  
    { d4 bes8 a4. | }
  }
  g bes4 a8 |
  bes[ f] c' f,4 c'8 |
  
  c2. |
  a4. bes a |
  d4 c8 c[ a] bes |
  a2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat volta 2 {
    f8 |
    f4 f8 bes,4 c8 |
    f( f) f8 e4 c8 |
    d4 d8 e[ d] c |
  }
  \alternative {
    { d4 e8 f4 | }
  
    { d4 e8 f4. | }
  }
  c g'4 d8 |
  c[ f] e f4 f8 |
  c2. |
  f4. bes, c |
  f4 f8 e[ d] c8 |
  <f f,>2. \bar "|."
  
  
  
  \repeat volta 2 {
    f8 |
    f4 f8 bes,4 c8 |
    f4 f8 e4 c8 |
    d4 d8 e[ d] c |
  }
  \alternative {
    { d4 e8 f4 | }
  
    { d4 e8 f4. | }
  }
  c g'4 d8 |
  c[ f] e f4 f8 |
  c2. |
  f4. bes, c |
  f4 f8 e[ d] c8 |
  <f f,>2. \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}
\score {
  \unfoldRepeats

  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 4)
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
    \tempo 4 = 150
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

