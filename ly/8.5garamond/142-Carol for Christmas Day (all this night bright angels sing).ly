\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Carol for Christmas Day"}}
  poet = \markup\oldStyleNum"William Austin (1587–1634)"
  composer = \markup\oldStyleNum"Sir Arthur S. Sullivan (1842–1900)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #142
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
  \key f \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(0.25 . 1)
  \override DynamicText.X-offset = #-3
}

sopMusic = \relative c' {
  f4^\mf g a bes |
  c4. d8 c2 |
  f,4 g a bes |
  c4. d8 c2 |
  c4.-> c8 c4 bes |
  
  a4.-> a8 a2 |
  g4^\markup\italic"cresc." g c e |
  g2 b, |
  c1 |
  g4^\p f bes2 |
  g g4 f |
  
  bes2 g |
  f4 g^\markup\italic"cresc." a bes |
  c4. d8 c2 |
  bes4 a g f |
  f2( g4) e |
  f1 \bar ":|." \break
  
  %verse 3
  f4^\p g a bes |
  c4. d8 c2 |
  f,4 g a bes |
  c4. d8 c2 |
  e4 d c c |
  
  b4. a8 gis2 |
  a4^\markup\italic"dim." a g e |
  g2^\pp g |
  g1 |
  g4 f^\markup\italic"cresc." bes2 |
  g g4 f |
  
  bes2 g |
  f4^\f g a bes |
  c4. d8 c2 |
  d4 <<g {s8 s^\markup\italic"rit."}>> f4 f, |
  a2 g |
  f1\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f4 f f g |
  a4. bes8 a2 |
  f4 f f f |
  e4. e8 f2 |
  a4. a8 a4 f |
  
  fis4. fis8 fis2 |
  g4 f e8[ g] c4 |
  c2 g |
  g1 |
  e4 f e2 |
  e e4 f |
  
  e2 e |
  f4 f f g |
  a4. a8 a2 |
  f4 fis g d |
  c2. c4 |
  f1 \bar ":|."
  
  %verse 3
  f4 f f f |
  g bes a2 |
  f4 f f f |
  g bes a2 |
  a4 gis a e |
  
  e4. e8 d2 |
  c4 d g, c |
  c2 b |
  c1 |
  e4 f e2 |
  e e4 f |
  
  e2 e |
  f4 f f g |
  a4. a8 a2 |
  bes4 g c, f |
  f2 e |
  f1 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsXI
  \set stanza = #"1. "
  \set associatedVoice = "basses" 
  All this night bright an -- gels sing,
  Nev -- er was such ca -- rol -- ing,
  Hark! a voice which loud -- ly cries,
  “Mor -- tals, mor -- tals, wake and \set associatedVoice = "sopranos" rise.
  Lo! to glad -- ness
  Turns your sad -- ness:
  From the earth is ris’n a Sun,
  Shines all night though day be done.”
  
  \set stanza = #"3. "
  \set associatedVoice = "tenors" 
  Hail! O Sun, O bless -- ed Light, Sent in -- to this world by night;
  Let Thy Rays and heav’n -- ly Pow’rs, \set associatedVoice = "sopranos" Shine in these dark souls of ours.
  For most du -- ly,
  Thou art tru -- ly
  \set associatedVoice = "sopranos"
  God and man, we do con -- fess: Hail, O Sun of Right -- eous -- ness!
}
altoWordsII = \lyricmode {
  \dropLyricsXI
%\markup\italic
  \set stanza = #"2. "
  \set associatedVoice = "basses" 
  Wake, O earth, wake ev -- ’ry -- thing,
  Wake and hear the joy I bring:
  Wake and joy; for all this night,
  Heav’n and ev -- ’ry twink -- ling \set associatedVoice = "sopranos" light,
  All a -- maz -- ing,
  Still stand gaz -- ing;
  An -- gels, Pow’rs, and all that be,
  Wake, and joy this Sun to see.
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
  a4^\mf bes c c |
  c4. c8 c2 |
  a4 bes c f, |
  g4. g8 a2 |
  ees'4. ees8 ees4 d |
  
  c4. c8 c2 |
  b4 b c c |
  d(^\f e) f2 |
  e1 |
  c4^\p c c2 |
  c c4 c |
  
  c2 c |
  a4 bes c d |
  ees4. ees8 ees2 |
  d4^\f c bes a |
  g( a) bes2 |
  a1 \bar ":|."
  
  %verse 3
  a4^\p bes c d |
  e c f2 |
  a,4 bes c d |
  e c f2 |
  c4^\mf b a e' |
  
  d4. c8 b2 |
  a4 b c g8[ e] |
  d4(^\pp e) f2 |
  e1 |
  c'4 c c2 |
  c c4 c |
  
  c2 c |
  a4^\f bes c d |
  ees4. ees8 ees2 |
  d4^\ff e f d |
  c2 bes |
  a1 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4 f f f |
  f4. f8 f2 |
  f4 f f d |
  c4. c8 f2 |
  f4. f8 f4 bes |
  
  d,4. d8 d2 |
  g4 g g g |
  g2 g |
  c,1 |
  bes'4 a g2 |
  c, bes'4 a |
  
  g2 c, |
  f4 f f f |
  f4. f8 f2 |
  bes,4 d g, a8[ bes] |
  c2 c |
  f1 \bar ":|."
  
  %verse 3
  f4 f f f |
  f4. f8 f2 |
  f4 f f f |
  f4. f8 f2 |
  e4 e e e |
  
  e4. e8 e2 |
  f4 f e c |
  g2 g |
  c1 |
  bes'4 a g2 |
  c, bes'4 a |
  
  g2 c, |
  f4 f f f |
  f4. f8 f2 |
  bes4 bes a bes |
  c2 c, |
  f1\fermata \bar "|."
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
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.9))} \lyricsto "altos" \altoWords
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

