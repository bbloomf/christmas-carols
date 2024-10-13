\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Wexford Carol"}}
  poet = \markup\oldStyleNum"Traditional, 16th Century or earlier"
  composer = \markup\oldStyleNum"Traditional"
  %arranger = \markup\oldStyleNum"Edited by Benjamin Bloomfield (1984–)"
  tagline = \markup { \center-column { \concat{"from "\italic"free-scores.com" ", with additional verses from"} \concat{\italic"Some Ancient Christmas Carols with the Tunes To Which They Were Formerly Sung in the West of England" \oldStyleNum", 1822,"} \concat{"via "\italic"books.google.com"}}}
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
  first-page-number = #056
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
  \time 3/4
  \autoBeamOff
  \slurSolid
}

sopMusic = \relative c' {
  \repeat volta 3 {
    \partial 8 d8 |
    g8 g16[ fis] g4. a8 |
    b c d4 b8\rest d |
    c b g( d4) e8 |
    
    f g f4 \bar"" \break b8\rest d, |
    g g16[ fis] g4. a8 |
    b c d4 b8\rest b |
    
    c d b( g4) a8 |
    g g g4 \bar"" \break b8\rest g |
    g8 f' d( c4) a8 |
    
    bes c bes4. d16[ c] |
    \times 2/3 {bes8[ a] g} g( d4) e8 |
    f g f4 \bar"" \break b8\rest d, |
    
    g8 g16[ fis] g4. a8 |
    b c d4 b8\rest b |
    c d \slurDotted b8( g4) a8 |
    \partial 8*5 g g g4. \break
  }
  
  \repeat volta 3 {
    \slurSolid
    \partial 8 d8 |
    g8 g16[ fis] g4. a8 |
    b c d4 b8\rest d |
    c b g( d4) e8 |
    
    f g f4 \bar"" \break b8\rest d, |
    g g16[ fis] g4. a8 |
    b c d4 b8\rest b |
    
    c d b( g4) a8 |
    g g g4 \bar"" \break b8\rest g |
    g8 f' d( c4) a8 |
    
    bes c bes4. d16[ c] |
    \times 2/3 {bes8[ a] g} g( d4) e8 |
    f g f4 \bar"" \break b8\rest d, |
    
    g8 g16[ fis] g4. a8 |
    b c d4 b8\rest b |
    c d b8( g4) a8 |
    \partial 8*5 g g g4.
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  g d d8.[ c16 b8] d |
  g g fis4 s8 d |
  e fis g( d4) e8 |
  
  c c d4 s8 d |
  g d d8.[ c16 b8] d |
  g g fis4 s8 e |
  
  e8 d d( f4) e8 |
  d c d4 s8 |
  g |
  g a g( e4) f8 |
  
  g8 c, d4. d8 |
  d d g( d4) e8 |
  d e d4 s8 d |
  
  g d d8.[ c16 b8] d |
  g g fis4 s8 e |
  e8 d \slurDotted d( f4) e8 |
  e d b4.
  
  
  \slurSolid
  d8 |
  g d d8.[ c16 b8] d |
  g g fis4 s8 d |
  e fis g( d4) e8 |
  
  c c d4 s8 d |
  g d d8.[ c16 b8] d |
  g g fis4 s8 e |
  
  e8 d d( f4) e8 |
  d c d4 s8 |
  g |
  g a g( e4) f8 |
  
  g8 c, d4. d8 |
  d d g( d4) e8 |
  d e d4 s8 d |
  
  g d d8.[ c16 b8] d |
  g g fis4 s8 e |
  e8 d d( f4) e8 |
  e d b4.
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \set associatedVoice = "tenors"
  Good peo -- ple all, __ this Christ -- mas time,
  Con -- sid -- er well, and bear in mind,
  \set associatedVoice = "basses"
  What our good God __ for us has done,
  In send -- ing His __ be -- lov -- ed Son.
  
  With Ma -- ry ho -- ly, we should pray
  To God with love this Christ -- mas day;
  In Beth -- le -- hem __ up -- on that morn,
  There was a
  \set ignoreMelismata = ##t
  bless -- ed Mes -- si -- ah born.
  \unset ignoreMelismata
  
  
  \set stanza = #"4. "
  \set associatedVoice = "tenors"
  Near Beth -- le -- hem __ did shep -- herds keep
  Their flocks of lambs and feed -- ing sheep;
  \set associatedVoice = "basses"
  To whom God’s an -- gels did ap -- pear,
  Which put the shep -- herds in great fear.
  “Pre -- pare and go,” the an -- gels said,
  “To Beth -- le -- hem. Be __ not a -- fraid
  For there you’ll find __ this hap -- py morn
  A prince -- ly babe sweet Je -- sus born.”
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  \set associatedVoice = "tenors"
  The night be -- fore __ that hap -- py tide,
  The no -- ble Vir -- gin and her guide
  \set associatedVoice = "basses"
  Were long time seek -- ing up and down
  To find a lodg -- ing in the town.
  But mark how all things came to pass:
  From ev -- ’ry door re -- pelled A -- las!
  As long fore -- told __ their ref -- uge all
  Was but a hum -- ble ox -- ’s stall.
  
  \set stanza = #"5. "
  \set associatedVoice = "tenors"
  With thank -- ful heart __ and joy -- ful mind,
  The shep -- herds went the  babe to find,
  \set associatedVoice = "basses"
  And as God’s an -- gels had fore -- told,
  They did our Sav -- ior Christ be -- hold.
  With -- in a man -- ger He was laid,
  And by His side the vir -- gin maid
  At -- tend -- ing on __ the Lord of Life
  Who came to earth to end all strife.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \set associatedVoice = "tenors"
  Let all your songs __ and prais -- es be,
  Un -- to His Heav’n -- ly __ Ma -- je -- sty;
  \set associatedVoice = "basses"
  And ev -- er -- more, __ a -- mongst our mirth,
  Re -- mem -- ber Christ our Sav -- ior’s birth.
  
  That night the Vir -- gin Ma -- ry mild,
  Was safe de -- liv -- er’d of a child;
  Ac -- cord -- ing un -- to Heav’n’s de -- cree,
  Man’s sweet sal -- va -- tion for to be.
  
  \set stanza = #"6. "
  \set associatedVoice = "tenors"
  See how the Lord __ of Heav’n and earth,
  Show’d Him -- self low -- ly __ in His birth;
  \set associatedVoice = "basses"
  A sweet ex -- am -- ple for man -- kind,
  To learn to bear __ a hum -- ble mind.
  
  If quires of An -- gels did re -- joice,
  Well may man -- kind with heart and voice
  Sing prais -- es to __ the God of Heav’n,
  That un -- to us __ His Son has giv’n.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
  
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c {
  d8 |
  g a b8.[ c16 d8] d |
  b g a4 s8 b |
  a fis b[ a] c[ b] |
  
  a g a4 s8 d, |
  g a b8.[ c16 d8] d |
  b g a4 s8 g |
  
  g g g4 c |
  b8 c b4 s8 |
  g |
  g c b( a4) c8 |
  
  d8 a f4. g8 |
  \times 2/3 {g[ a] bes} bes[ a] bes[ c] |
  a c a4 r8 d, |
  
  g a b8.[ c16 d8] d |
  b g a4 s8 g |
  g g \slurDotted g8( g4) c8 |
  b c d4.
  
  
  
  \slurSolid
  d,8 |
  g a b8.[ c16 d8] d |
  b g a4 s8 b |
  a fis b[ a] c[ b] |
  
  a g a4 s8 d, |
  g a b8.[ c16 d8] d |
  b g a4 s8 g |
  
  g g g4 c |
  b8 c b4 s8 |
  g |
  g c b( a4) c8 |
  
  d8 a f4. g8 |
  \times 2/3 {g[ a] bes} bes[ a] bes[ c] |
  a c a4 r8 d, |
  
  g a b8.[ c16 d8] d |
  b g a4 s8 g |
  g g g8( g4) c8 |
  b c d4.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 |
  g d b8.[ a16 g8] fis' |
  e e d4 d8\rest b |
  c d e[ fis] g4 |
  
  f8 e d4 d8\rest d |
  g d b8.[ a16 g8] fis' |
  e e d4 d8\rest e |
  
  c b b[ d] c4 |
  g'8 e g4 d8\rest g |
  g f g( a4) f8 |
  
  g f bes,4. g8 |
  g' g g[ f] g[ a] |
  d, c d4 g, |
  
  g'8 d b8.[ a16 g8] fis' |
  e e d4 d8\rest e |
  c b \slurDotted b( d4) c8 |
  e g g4.
  
  
  
  \slurSolid
  d8 |
  g d b8.[ a16 g8] fis' |
  e e d4 d8\rest b |
  c d e[ fis] g4 |
  
  f8 e d4 d8\rest d |
  g d b8.[ a16 g8] fis' |
  e e d4 d8\rest e |
  
  c b b[ d] c4 |
  g'8 e g4 d8\rest g |
  g f g( a4) f8 |
  
  g f bes,4. g8 |
  g' g g[ f] g[ a] |
  d, c d4 g, |
  
  g'8 d b8.[ a16 g8] fis' |
  e e d4 d8\rest e |
  c b b( d4) c8 |
  e g g4.
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
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
    \tempo 4 = 65
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
