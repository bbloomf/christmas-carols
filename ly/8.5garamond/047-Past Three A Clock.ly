\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Past Three a Clock"}}
  poet = \markup\oldStyleNum"George Ratcliffe Woodward (1848–1934)"
  composer = \markup{\italic"London Waits"}
  arranger = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
  tagline = \markup \concat{ "from " \italic "The Cambridge Carol Book" \oldStyleNum", 1924"}
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
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
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #047
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
  \key g \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \repeat volta 2 {
    g'4 e e |
    d2 d8 d |
    g[ a] b4 b ||
    d2 a4 |
    g e e |
    d2 d4 | \break
    
    g8 a b4 a |
    g2.\fermata \bar"||"
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
%    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \mark\markup\italic"Fine"
  
    g4 a b |
    a2 g4 |
    a g a |
    b g2 |
    
    g8[ fis] g[ a] b[ c] |
    a2 g4 |
    a g a |
    b e2\fermata
  }
    
  g,4 a b |
  a2 g4 |
  a g a |
  b g2 |
  
  g8[ fis] g[ a] b[ c] |
  a2 g4 |
  a g a |
  b e2\fermata \bar"||"
  \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
  \mark"D.C."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 c c |
  a2 d8 d |
  d4 d d |
  d2 d4 |
  b c c |
  a2 d4 |
  
  d8 d d4 d |
  b2.
  
  d4 fis g |
  g( fis) g |
  d e fis |
  g d2 |
  
  d4 e g |
  g( fis) g |
  d e fis |
  g g2
  
  
  
  d4 fis g |
  g( fis) g |
  d e fis |
  g d2 |
  
  d4 e g |
  g( fis) g |
  d e fis |
  g g2 \bar "||"
}
dropLyrics = {
  \override LyricText.extra-offset = #'(0 . -4.9)
  \override LyricHyphen.extra-offset = #'(0 . -4.9)
  \override LyricExtender.extra-offset = #'(0 . -4.9)
  \override StanzaNumber.extra-offset = #'(0 . -4.9)
}
altoWords = \lyricmode {
  \dropLyricsIV
  Past three a clock,
  And a cold frost -- y morn -- ing,
  Past three a clock;
  Good
  \dropLyrics
  mor -- row, mas -- ters all!
  
  \dropLyricsVII
  \set stanza = #"1. "
  \set associatedVoice = "sopranos"
  Born is a Ba -- by, Gen -- tle as may be,
  Son of th’e -- ter -- nal Fa -- ther su -- per -- nal.
  
  
  \set stanza = #"5."
  Cheese from the dai -- ry
  Bring they for Ma -- ry,
  And, not for mon -- ey,
  But -- ter and hon -- ey.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
%\markup\italic
  \repeat unfold 21 {\skip1}
  \set stanza = #"2. "
  Ser -- aph quire sing -- eth, An -- gel bell ring -- eth;
  Hark how they rime it, Time it, and chime it.
  
  
  \set stanza = #"6."
  Light out of star -- land
  Lead -- eth from far land
  Prin -- ces, to meet Him,
  Wor -- ship and greet Him.
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \repeat unfold 21 {\skip1}
  \set stanza = #"3. "
  Mid earth re -- joic -- es
  Hear -- ing such voi -- ces
  Ne’er -- to -- fore so well
  Ca -- rol -- ling No -- ël.
  
  
  \set stanza = #"7."
  Myrrh from full cof -- fer,
  In -- cense they of -- fer;
  Nor is the gol -- den
  Nug -- get with -- hol -- den.
}
altoWordsIV = \lyricmode {
  \dropLyricsVII
  \repeat unfold 21 {\skip1}
  \set stanza = #"4. "
  Hinds o’er the pear -- ly
  Dew -- y lawn ear -- ly
  Seek the high Stran -- ger
  Laid in the man -- ger.
  
  
  \set stanza = #"8."
  Thus they: I pray you,
  Up, sirs nor stay you
  Till ye con -- fess Him
  Like -- wise, and bless Him.
}
altoWordsV = \lyricmode {
  \repeat unfold 21 {\skip1}
}
altoWordsVI = \lyricmode {
  \repeat unfold 21 {\skip1}
}
altoWordsVII = \lyricmode {
  \repeat unfold 21 {\skip1}
}
altoWordsVIII = \lyricmode {
  \repeat unfold 21 {\skip1}
}
tenorMusic = \relative c' {
  b4 g a |
  fis2 fis8 fis |
  g4 g8[ a] b4 |
  a( g) fis |
  g g a8[ g] |
  fis2 fis4 |
  
  g8 g g4 fis |
  g2. |
  
  d'4 c b |
  d2 b4 |
  a b d |
  d b2 |
  
  b4 b e |
  d2 b4 |
  a b d |
  d c2
  
  
  
  d4 c b |
  d2 b4 |
  a b d |
  d b2 |
  
  b4 b e |
  d2 b4 |
  a b d |
  d c2 \bar "||"
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g4 c a |
  d2 d8 c |
  b[ a] g4 g' |
  fis( e) d |
  e c a |
  d2 d8[ c] |
  
  b a g4 d' |
  g2.\fermata |
  
  b4 a g |
  d2 e4 |
  fis e d |
  g g,2 |
  
  g'4 e c |
  d2 e4 |
  fis e d |
  g c,2\fermata
  
  
  
  
  
  b'4 a g |
  d2 e4 |
  fis e d |
  g g,2 |
  
  g'4 e c |
  d2 e4 |
  fis e d |
  g c,2\fermata \bar "||"
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
    \new Lyrics = "altosVIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVIII
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . 0))} \lyricsto "altos" \altoWords
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
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/4)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
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

