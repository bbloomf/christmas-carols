\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Virgin and Child"}}
  poet = \markup\oldStyleNum{\concat{"Adapted from " \italic"Thys endris nyzth" \oldStyleNum", 15th Century"}}
  composer = \markup\oldStyleNum"Charles Steggall (1826–1905)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 8)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 2)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #086
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
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(1 . 1)
  \override DynamicText.X-offset = #-2.5
}

sopMusic = \relative c'' {
  \tempo 4 = 112
  \partial 4 g4 |
  c4. a8 b4 g |
  c4. a8 b4 g |
  c a8[ b] c4 f |
  d2~ d4 g, |
  c4. a8 b4 g |
  
  c4.^\markup\italic"dim." a8 b4_\p g |
  a e8[ f] g4 c,8[ d] |
  e4 f8[ d] e4 c8[ d] |
  e2. c8[ d] |
  \partial 2. e2.\fermata | \break
  
  \repeat volta 2 {
    \partial 4 e4 |
    a4. b8 c4 d |
    b4. c8 a4 b |
    c4 a8[ b] c4 f |
    e2. e4 |
    
    d4 e8[ d] c[ d] e4 |
    a, c b a8[ b] |
    c[ b] a4 b4. c8 |
    a2. a4 |
    
    g4 e g a |
    g e g a8[ b] |
    c4 a d g, |
    e'2. e4 |
    
    b^\markup\italic"dim." c g a |
    e4. f8 g4 \bar"||"\break a8[ b] |
    c4 d8[ e] c4 a8[ b] |
    g[ f] g[ a] g4 a8[ b] |
    
    c[ a] g[ a] g4 b\rest |
    b2\rest d4\rest c4 |
    a^\markup\italic"cresc." e g d8[ e] |
    c2. c4 |
    
    e4 f8[ d] e4 c |
    e f8[ d] e4 c8[ d] |
    e4 c8[ d] e4 c |
    \partial 2. e2.
  }
}
sopWords = \lyricmode {
  \repeat unfold 26 { \skip 1 }
  lul -- lay, lul -- lay, lul -- lay,
  \repeat unfold 24 { \skip 1 }
  It makes my heart to ache,
  \repeat unfold 8 { \skip 1 }
  A King up -- on this hay;
  \repeat unfold 24 { \skip 1 }
  lul -- lay, lul -- lay, lul -- lay,
  lul -- lay, by by,
}

altoMusic = \relative c' {
  g'4 |
  e4. f8 d4 g |
  e4. f8 d4 g |
  e f g f |
  g( f8[ a] g4) g |
  e4. f8 d4 g |
  
  e4. f8 d4 g |
  f c c a |
  c2~ c4 e,\rest |
  g\rest c4 b a |
  b2. |
  
  e4 |
  a4. b8 c4 d |
  b4. c8 a4 gis |
  a e e g |
  g2. g4 |
  
  g b a a8[ g] |
  f[ g] a4 gis e |
  e( a) e2 |
  c4 f8[ d] e4 e |
  
  d c d f |
  e c d d |
  g( e) g4.( f8) |
  e4 f8[ a] g4 g |
  
  g8[ f] e4 e8[ d] c4 |
  c c d f8[ d] |
  e4 f8[ g] e4 f8[ d] |
  e[ d] e[ f] e4 f8[ d] |
  
  e[ f] e[ f] e4 c8[ d] |
  e[ d] e[ f] e4 e |
  c c c b |
  c d8[ e] c4 a8[ b] |
  
  c2. a8[ b] |
  c2. a8[ b] c4 a c a |
  c2.
}
dropLyrics = {
  \override LyricText.extra-offset = #'(0 . -1.8)
  \override LyricHyphen.extra-offset = #'(0 . -1.8)
  \override LyricExtender.extra-offset = #'(0 . -1.8)
  \override StanzaNumber.extra-offset = #'(0 . -1.8)
}
altoWords = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic"  mf" " 1."}
  \lyricmode {
    On yes -- ter night I saw a sight,
    A star as bright as day; __
    And all a -- long, I heard a song,
    lul -- lay, by by, \dropLyricsIV lul -- lay, __
    \dropLyricsXV lul -- lay, lul -- lay.
  }
  \dropLyricsIX
  \set stanza = "2."
  \lyricmode{
    A love -- ly la -- dy sat and sang,
    And to her Child she spake:
    My Son, my Bro -- ther, Fa -- ther dear,
    It makes my heart to ache,
    To see Thee there, so cold and bare,
    A King up -- on this hay;
    But hush Thy wail, I will not fail
    \dropLyricsVI
    To sing by by, lul -- lay, lul -- lay,
    to sing by by, \dropLyrics lul -- lay, lul -- lay;
    To sing by by lul -- lay, lul -- lay,
    lul -- lay, \dropLyricsVI lul -- lay,
    \dropLyrics
    lul -- lay, lul -- lay, lul -- lay.
  }
}
altoWordsII = {
  \dropLyricsIX
  \lyricmode{
    \repeat unfold 32 \skip1
  }
  \set stanza = \markup{\dynamic"  f " "3."}
  \lyricmode {
  %\markup\italic
    The Child then spake whilst she did sing,
    And to the maid -- en said:
    “Right sure I am a might -- y King,
    Though in a crib My bed:
    For an -- gels bright,
    Down to Me light;
    Thou canst not say Me nay:
    Then why so sad?
    Thou may’st be glad
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \repeat unfold 32 \skip1
  \set stanza = #"4."
  “Now, sweet -- est Lord, since Thou art King,
  Why liest Thou in a stall?
  Why didst Thou not Thy cra -- dle bring
  To some great roy -- al hall?
  Me -- thinks ’tis right,
  That king or knight
  Should lie in good ar -- ray;
  And them a -- mong,
  It were no wrong
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
  g4 |
  g4 c d b |
  g c d b |
  c c8[ d] e4 c |
  b( a8[ c] b4) g |
  g c d b |
  
  g c d^\p e |
  c a g a |
  g a8[ f] g4 s |
  s a gis a |
  gis!2. |
  
  e4 |
  a4. b8 c4 d |
  b4. c8 a4 e' |
  e e c b |
  c2. c4 |
  
  b g' e e |
  d c8[ d] e4 e |
  a,2 a4( gis) |
  a d8[ b] c4 c |
  
  b g8[ a] b4 a8[ b] |
  c4 g8[ a] b4 d |
  c2 b4( d) |
  c d8[ f] e4 c |
  
  e4 e8[ d] c4 f |
  a,4 a b g |
  g2. g4 |
  g2. g4 |
  
  g4 c c a8[ b] |
  c[ b] c[ d] c4 g |
  f c' g g |
  e f8[ g] e4 a |
  
  g a8[ f] g4 a |
  g a8[ f] g4 a |
  g f g f |
  g2.
}
tenorWords = \lyricmode {
  \repeat unfold 26 { \skip 1 }
  lul -- lay, lul -- lay
  \repeat unfold 4 { \skip 1 }
  \repeat unfold 28 { \skip 1 }
  \repeat unfold 14 { \skip 1 }
  \repeat unfold 8 { \skip 1 }
  To sing by by, lul -- lay,
  by by,
  \repeat unfold 14 { \skip 1 }
  by by lul -- lay, by by
}

bassMusic = \relative c {
  g'4 |
  c,4. c8 c4 c |
  c4. c8 c4 c |
  c f c a' |
  g2~ g4 g |
  c,4. c8 c4 c |
  
  c4. c8 c4 c |
  f a e f |
  c2~ c4 d\rest |
  d\rest a e' a |
  e2.\fermata |
  
  e4 |
  a4. b8 c4 d |
  b4. c8 a4 e |
  a c a g |
  c,2. c'4 |
  
  g4 e a c, |
  d a e' fis8[ gis] |
  a4 c, e e |
  a,2. a4 |
  
  b4 c g f |
  c' c g f' |
  e a g b, |
  c2. c'4 |
  
  g a e f8[ g] |
  a4. a8 g4 g |
  c,2. c4 |
  c2. g'4 |
  
  c,2. f4 |
  c2. c4 |
  f a e4 g |
  c,2. f4 |
  
  c2. f4 |
  c2. f4 |
  c f c f |
  c2.
}
bassWords = \lyricmode {
  \repeat unfold 26 { \skip 1 }
  lul -- lay
  \repeat unfold 4 { \skip 1 }
  \repeat unfold 22 { \skip 1 }
  It makes my heart to ache,
  \repeat unfold 8 { \skip 1 }
  A King up -- on this hay;
  \repeat unfold 14 { \skip 1 }
  lul -- lay. To sing by by, lul -- lay,
  lul -- lay, lul -- lay,
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1) (padding -1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1) (padding . 0)) } \lyricsto "basses" \bassWords
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
\markup {\fontsize #0.8 \override #'(font-name . "Garamond Premier Pro")
  \fill-line {
    \hspace #0.1 % moves the column off the left margin;
        % can be removed if space on the page is tight
     \column {
      \line { \bold "5."
        \column {
          "“My Mother Mary, thine I be,"
          "     Though I be laid in stall,"
          "Both lords and dukes shall worship Me,"
          "     And so shall monarchs all:"
          "          Ye shall well see"
          "          That princes three,"
          "     Shall come on the twelfth day:"
          "          Then let Me rest"
          "          Upon thy breast,"
          "     And sing by by, lullay.”"
        }
      }
    }
    \hspace #0.1  % adds horizontal spacing between columns;
        % if they are still too close, add more " " pairs
        % until the result looks good
     \column {
      \line { \bold "6."
        \column {
          "“Now tell me, sweetest Lord, I pray,"
          "     Thou art my love and dear,"
          "How shall I nurse Thee to Thy mind,"
          "     And make Thee glad of cheer?"
          "          For all Thy will"
          "          I would fulfil,"
          "     I need no more to say;"
          "          And for all this"
          "          I will Thee kiss,"
          "     And sing by by, lullay,”"
        }
      }
    }
    \hspace #0.1 % gives some extra space on the right margin;
      % can be removed if page space is tight
  }
}

\markup {
  \vspace #0.7 % adds vertical spacing between verses\
  \override #'(font-name . "Garamond Premier Pro")
  \fill-line {\override #'(font-name . "Garamond Premier Pro")
    " "
    \column {\fontsize #0.8
      \line { \bold "7."
        \column {
          "“My Mother dear, when time it be,"
          "     Then take Me up aloft,"
          "And set Me up upon thy knee,"
          "     And handle Me full soft;"
          "          And in thy arm,"
          "          Thou wilt Me warm,"
          "     And keep Me night and day:"
          "          And if I weep,"
          "          And may not sleep,"
          "     Thou sing by by, lullay.”"
        }
      }
    }
    " "
  }
}

