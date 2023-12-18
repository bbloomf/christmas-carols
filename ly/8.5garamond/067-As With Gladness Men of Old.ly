\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"As With Gladness Men of Old"}}
  poet = \markup\oldStyleNum"William C. Dix (1837–1898)"
  composer = \markup\oldStyleNum"Konrad Kocher (1786–1872)"
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #067
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
  \key aes \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \tempo 4 = 100
  aes4 g8[ aes] bes4 aes |
  des des c2 |
  f,4 g aes f | \break
  
  ees4 ees ees2 |
  aes4 g8[ aes] bes4 aes |
  des des c2 | \break
  
  f,4 g aes f |
  ees ees ees2 |
  c'4 bes aes c | \break
  
  ees4. des8 c2 |
  f,4 g aes des |
  c bes aes2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 ees ees ees |
  des ees ees2 |
  des4 ees ees des |
  
  c bes c2 |
  ees4 ees ees ees |
  des ees ees2 |
  
  des4 ees ees des |
  c bes c2 |
  ees4. des8 c4 f |
  
  ees ees ees2 |
  des4 ees ees des |
  ees4. des8 c2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  As with __ glad -- ness men of old
  Did the guid -- ing star be -- hold;
  As with __ joy they hail’d its light,
  Lead -- ing on -- ward, beam -- ing bright;
  So, most gra -- cious God, may we
  Ev -- er -- more be led to Thee.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  As with __ joy -- ful steps they sped
  To that low -- ly man -- ger -- bed,
  There to __ bend the knee be -- fore
  Him whom heav’n and earth a -- dore;
  So may we with will -- ing feet
  Ev -- er seek Thy mer -- cy seat.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  As they __ of -- fer’d gifts most rare
  At that man -- ger rude and bare;
  So may __ we with ho -- ly joy,
  Pure and free from sin’s al -- loy,
  All our cost -- liest trea -- sures bring,
  Christ, to Thee, our heav’n -- ly King.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Ho -- ly __ Je -- sus, ev -- ’ry day
  Keep us in the nar -- row way;
  And, when __ earth -- ly things are past,
  Bring our ran -- somed souls at last
  Where they need no star to guide,
  Where no clouds Thy glo -- ry hide.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"5. "
  In the __ heav’n -- ly coun -- try bright
  Need they no cre -- a -- ted light;
  Thou its __ Light, its Joy, its Crown,
  Thou its Sun which goes not down;
  There for -- ev -- er may we sing
  Al -- le -- lu -- ias to our King.
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  c4 des8[ c] bes4 c |
  aes bes aes2 |
  aes4 bes aes aes |
  
  aes g aes2 |
  c4 des8[ c] bes4 c |
  aes bes aes2 |
  
  aes4 bes aes aes |
  aes g aes2 |
  aes4 g aes aes |
  
  bes g aes2 |
  aes4 bes aes aes |
  aes g aes2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  aes4 bes8[ aes] g4 aes |
  f g aes2 |
  des,4 des c des |
  
  ees ees aes,2 |
  aes'4 bes8[ aes] g4 aes |
  f g aes2 |
  
  des,4 des c des |
  ees ees aes,2 |
  aes'4 ees f f |
  
  g ees aes2 |
  des,4 des c f |
  ees ees aes,2 \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.7)) } \lyricsto "tenors" \altoWords
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
}

