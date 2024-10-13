\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Carol for Christmas Eve"}}
  poet = \markup\oldStyleNum"Rev. H. R. Bramley (1833–1917)"
  composer = \markup\oldStyleNum"Sir Frederick A. G. Ouseley (1825–1889)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
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
  %     (padding . -5)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #026
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
  \key c \major
  \time 2/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \repeat volta 2 {
    c8 c' b a |
    g8. e16 c8 d |
    e16[ f] g8 e c' |
    b2 |
    a8 b c e | 
    
    d fis, g a |
    b c a8. g16 |
    g4 b\rest |
    d8 d e8. e16 |
    e8. d16 d8 g, | 
    
    c d e d16[ c] |
    d4 b\rest |
    d8. d16 d8 c |
    b8. c16 a8 e |
    f a a gis | 
    
    \partial 4. a b\rest b\rest \bar "||" \break
    \partial 8 b |
    c b\rest b\rest g |
    e b'\rest b\rest g |
    a g f g |
    a g e4 | \break
    
    c'8 c16 c f8 e |
    d4 d |
    e8 d c b |
    a g f e |
    a16[ b] c8 d8.-> c16 |
    c4 b\rest \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c8 c' b a |
  g8. e16 c8 b |
  c d e fis |
  g2 |
  e8 g g a16[ g] |
  
  fis8 d16[ c] b8 e |
  d c e d16[ c] |
  b4 s |
  g'8 g g8. g16 |
  g8. g16 g8 g |
  
  g g g g |
  g4 s |
  f8. f16 e8 e |
  d8. e16 c8 e |
  d d e e |
  
  e s s g |
  g s s e |
  c s s e |
  e e d e |
  f e16[ d] c4 |
  
  a'8 a16 a a8 g |
  g4 g |
  g8 g e e |
  c c c16[ b] c8 |
  f g f8. e16 |
  e4 s
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  Lis -- ten, Lord -- ings, un -- to me, a tale I will you tell;
  Which, as on this night of glee, in Da -- vid’s town be -- fell.
  Jo -- seph came from Na -- za -- reth, with Ma -- ry that sweet maid:
  Wea -- ry were they, nigh to death; and for a lodg -- ing pray’d.
  
  \set associatedVoice = "tenors"
  Sing high, sing high,
  \set associatedVoice = "basses" sing low,
  sing low, sing high, sing low,
  \unset associatedVoice
  sing to and fro,
  Go tell it out with speed, Cry out and shout all round a -- bout,
  That Christ is born in -- deed.
  
  
  \set stanza = #"3. "
  Shep -- herds lay a -- field that night, to keep the sil -- ly sheep,
  Hosts of An -- gels in their sight came down from heav’n’s high steep.
  Ti -- dings! Ti -- dings! un -- to you: to you a Child is born,
  Pur -- er than the drops of dew, and bright -- er than the morn.
  
  \set associatedVoice = "tenors"
  Sing high, sing high,
  \set associatedVoice = "basses" sing low,
  sing low, sing high, sing low,
  \unset associatedVoice
  sing to and fro,
  Go tell it out with speed, Cry out and shout all round a -- bout,
  That Christ is born in -- deed.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
%\markup\italic
  \set stanza = #"2. "
  In the Inn they found no room; a scan  -- ty bed they made:
  Soon a Babe from Ma -- ry’s womb was in the man -- ger laid.
  Forth He came as light through glass: He came to save us all.
  In the sta -- ble ox and ass be -- fore their Ma -- ker fall.
  
  \repeat unfold 32 \skip1
  
  \set stanza = #"4. "
  On -- ward then the An -- gels sped, the shep -- herds on -- ward went,
  God was in His man -- ger bed, in wor -- ship low they bent.
  In the morn -- ing see ye mind, my mas -- ters one and all,
  At the Al -- tar Him to find, Who lay with -- in the stall.
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
tenorMusic = \relative c {
  c8 c' b a |
  g8. e16 c8 g' |
  g g g a |
  d2 |
  c8 d c c16[ b] |
  
  a8 b16[ a] g8 g |
  g g g fis |
  g4 s |
  b8 b b8. c16 |
  c8. b16 b8 g |
  
  a b c b16[ a] |
  b4 s |
  a8. a16 a8 a |
  gis8. gis16 a8 a |
  a a b b |
  
  c8 s s d |
  e c c c |
  g c\rest c\rest c |
  c c a c |
  c b c4 |
  
  c8 c16 c c8 c |
  b4 b |
  c8 b c g |
  a e f g |
  c e b8. c16 |
  c4 s
}
tenorWords = \lyricmode {
}
tenorWordsBass = \lyricmode {
}

bassMusic = \relative c {
  c8 c' b a |
  g8. e16 c8 g |
  c b c a |
  g2 |
  g'8 f e c |
  
  d d e c |
  d e c d |
  g,4 d'\rest |
  g8 g e8. c16 |
  g'8. g16 g8 g |
  
  e d c c |
  g'4 d\rest |
  d8. d16 a8 a |
  e'8. e16 f8 c |
  d f e e |
  
  a d,\rest d\rest g |
  c d,\rest d\rest c |
  c c c c |
  a c d c |
  f g a4 |
  
  f8 f16 f f8 c' |
  g4 g |
  c8 g a e |
  f c d e |
  f c g8. c16 c4 d\rest
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold 2 \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold 2 \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 2 \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
%    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
    \tempo 4 = 80
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
