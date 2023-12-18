\version "2.24.0"
\include "util.ly"
\header {
  tagline = \markup \concat{ "from " \italic "The Cambridge Carol Book" \oldStyleNum", 1924"}
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #160
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
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "basses" \altoWords
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"To Us This Morn a Child is Born"}}
    poet = \markup\oldStyleNum"George Ratcliffe Woodward (1848–1934)"
    composer = \markup{\italic "Jog on, jog on the footpath way"}
    arranger = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
  }
}







%When Angelick Host Entuned
global = {
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  g'4 g a a |
  b b g2 |
  a4 b c d |
  b2 a |
  
  e4 fis g2 |
  e4 fis g2 |
  a4 g fis g |
  e2 d\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 g g fis |
  g g e2 |
  e4 e e f |
  e2 e |
  
  e4 d8[ c] b2 |
  e4 d8[ c] b2 |
  e4 d d d~ |
  d cis d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  When an An -- gel host en -- tuned
  An -- them sweet and ai -- ry
  O’er the Child, meek and mild,
  \set associatedVoice = "sopranos"
  Of the Vir -- gin Ma -- ry;
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  When, with hon -- ey, herd -- men brought
  But -- ter from the dai -- ry
  To the One Ho -- ly Son
  \set associatedVoice = "sopranos"
  Born of Maid -- en Ma -- ry;
  \set ignoreMelismata = ##t
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  When three pil -- grim kings un -- lockt
  Each his cas -- ket, spa -- ry
  Of no thing for this King,
  \set associatedVoice = "sopranos"
  God, the Son of Ma -- ry.
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  ‘Glo -- ry be to God on high,
  God, who can -- not va -- ry!’
  Was the lay on that day
  \set associatedVoice = "sopranos"
  Sung by Bless -- èd Ma -- ry.
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
  b4 b e d |
  d d c2 |
  c4 b a a~ |
  a gis a2 |
  
  c4 a g2 |
  c4 a g2 |
  c4 b a b |
  a2 fis \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 e c d |
  g g c2 |
  a4 gis a d, |
  e2 a, |
  
  a'4 d, e2 |
  c4 d e2 |
  a,4 b d g, |
  a2 d\fermata \bar "|."
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"When Angelick Host Entuned"}}
    poet = \markup\oldStyleNum"George Ratcliffe Woodward (1848–1934)"
    composer = \markup\concat{\italic"Heinz, wiltu Christa han" \oldStyleNum", 1582"}
    arranger = \markup\oldStyleNum"Arranged by George Ratcliffe Woodward (1848–1934)"
  }
}

