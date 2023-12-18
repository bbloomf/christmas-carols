\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Sleep, Holy Babe!"}}
  poet = \markup\oldStyleNum"Edward Caswall (1814–1878)"
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823–1876)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
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
       (padding . -20)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #101
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
  \key ees \major
  \time 4/2
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
%Introduction
  ees4_\p g bes c~c2 f, |
  d4 f aes bes bes2 ees, |
  bes'2\rest bes4\rest <aes d,>~ q <g ees> f <ees ces> |
  <ees bes g> bes'\rest bes2\rest bes1\rest |
  ees,2-> d2 bes'1\rest \bar ".|:-|" 
%
  ees,1^\pp ees2. aes4 |
  g1.^\markup\italic"cresc." ees'2 |
  ees d4( c) bes2. aes4 |
  g1. \bar "||" 
  
  g2^\mf |
  g2. g4 a2 b |
  d2. g,4 g2 g |
  g ees' ees d4( c) |
  c2. bes4 bes2 \bar "||" 
  
  bes2 |
  bes2.^\markup\italic"dim." ees,4 ees2 ees |
  ees1. \bar "||"
  d2^\pp |
  ees4( g) bes( c) bes2. d,4 |
  << { ees1( ees) }
     { s1. <ees des g,>2_\markup\smallCapsOldStyle"Accomp." }
  >> |
  <ees c aes>1 <ees ces aes> |
  <ees bes g>1 bes'1\rest \bar ":|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
%Introduction
  s1 ees |
  s1 des |
  s2. ces4~ ces bes c aes |
  s1*2 |
  aes1 s1 \bar ".|:-|"
%
  bes1 c2. ees4 |
  ees1 g |
  aes2 aes g f |
  f1. 
  
  f2 f2. f4 f2 f |
  f2. f4 f2 f |
  ees2. g4 g2 f4( ees) |
  ees2. d4 d2 |
  
  d2 |
  ees4( bes2) c4 des2 c4( bes) |
  c1( ees2) |
  d4( c) |
  bes2 ees ees4( d c) bes |
  bes4( ees d c bes1) |
}
altoWords = \lyricmode {
  \dropLyricsIX
  \repeat unfold 7 { \skip 1}
  \set stanza = #"1. "
  Sleep, Ho -- ly Babe!
  up --
  \set associatedVoice = "tenors"
  on Thy moth -- er’s breast;
  
  \set associatedVoice = "sopranos"
  Great Lord of earth, and sea, and sky, How sweet it is to see Thee lie
  In such a place of rest,
  In such a place of rest. __ 
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \repeat unfold 7 { \skip 1}
  \set stanza = #"2. "
  Sleep, Ho -- ly Babe!
  Thine 
  \set associatedVoice = "tenors"
  An -- gels watch a -- round,
  
  \set associatedVoice = "sopranos"
  All bend -- ing low with fold -- ed wings, Be -- fore th’In -- car -- nate King of kings,
  In rev -- ’rent awe pro -- found,
  In rev -- ’rent awe pro -- found. __
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \repeat unfold 7 { \skip 1}
  \set stanza = #"3. "
  Sleep, Ho -- ly Babe!
  while 
  \set associatedVoice = "tenors"
  I with Ma -- ry gaze,
  
  \set associatedVoice = "sopranos"
  In joy up -- on that Face a -- while, Up -- on the lov -- ing in -- fant smile
  Which there di -- vine -- ly plays,
  Which there di -- vine -- ly plays. __
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \repeat unfold 7 { \skip 1}
  \set stanza = #"4. "
  Sleep, Ho -- ly Babe!
  ah! 
  \set associatedVoice = "tenors"
  take Thy brief re -- pose;
  
  \set associatedVoice = "sopranos"
  Too quick -- ly will Thy slum -- bers break,
  And Thou to length -- en’d pains a -- wake,
  That death a -- lone shall close,
  That death a -- lone shall close. __
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
%Introduction
  s1 a |
  s1 g |
  s1*2 |
  ees4 g bes c~ c2 bes~ |
  bes1 bes \bar ".|:-|"
%
  g1_\pp aes2. c4 |
  bes1._\markup\italic"cresc." b2 |
  c2 d4( ees) ees2 d |
  d1. 
  
  d2_\mf |
  b2. b4 c2 d |
  b2. b4 c2 d |
  c4( d) c( bes) a( f) g( a) |
  a2. bes4 bes2 |
  
  d4( c) |
  bes2._\markup\italic"dim." bes4 bes2 ees, |
  ees1. |
  f4(_\pp fis) |
  g2 g aes aes |
  << { g4( c bes aes g1) }
     { s1. ees,2 }
  >> |
  aes4 c ees f f2 ees2~ |
  ees1 s1 \bar ":|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
%Introduction
  d1\rest f, |
  d'\rest ees, |
  aes4 c ees f~ f g aes aes, |
  bes bes\rest d2\rest d1\rest |
  bes1 d\rest \bar ".|:-|"
%
  ees1 ees2. ees4 |
  ees1 ees |
  aes,2 aes bes bes' |
  b1. 
  
  b!2 |
  g2. g4 g2 g, |
  g2. g4 a2 b |
  c c f2. f4 |
  f2. bes,4 bes2 |
  
  aes2 |
  g2. g4 g2 g2 |
  aes1. |
  a2 |
  bes bes bes bes |
  ees1~ ees |
  c,1\rest aes' |
  ees d'\rest \bar ":|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
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
      % a little smaller so lyrics
      % can be closer to the staff
      \Staff
      \override VerticalAxisGroup.minimum-Y-extent = #'(-3 . 3)
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

