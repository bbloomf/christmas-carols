\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Riu Riu Chiu"}}
  composer = \markup\oldStyleNum"Mateo Flecha el Viejo (1481–1553)"
  tagline = \markup { "from" \italic {cpdl.org}}
}
\paper {
  %print-all-headers = ##t
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
  first-page-number = #178
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
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  s4*26 |
  
  %\once \override Score.RehearsalMark.self-alignment-X = #LEFT
  \mark \markup { \abs-fontsize #11 \musicglyph "scripts.segno" }
  a'8 a g a |
  f4 e8 d4 e8 f g |
  a4 a b\rest |
  b\rest d\rest |
  a8 a g a |
  
  f4 e8 f4 f8 e e |
  d4 d b'\rest |
  b\rest d\rest |
  a8 a g a |
  f4 e8 d4 d8 c c |
  d4 d \bar "||"
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark "Fine"
}
sopWords = \lyricmode {
  \repeat unfold 12 {\skip1}
  Dios guar -- dó el
  \repeat unfold 8 {\skip1}
  Dios guar -- dó el
}

altoMusic = \relative c' {
  s4*26 |
  
  f8 f e c |
  d4 e8 f4 e8 d d |
  c4 c s |
  s e8 e |
  d c e c |
  
  d4 c8 d4 d8 c c |
  d4 d s |
  s e8 e |
  d c e c |
  d4 c8 a4 a8 a a |
  a4 a \bar "|."
}

dropLyrics = {
  \override LyricText.extra-offset = #'(0 . -2.0)
  \override LyricHyphen.extra-offset = #'(0 . -2.0)
  \override LyricExtender.extra-offset = #'(0 . -2.0)
  \override StanzaNumber.extra-offset = #'(0 . -2.0)
}
altoWords = \lyricmode {
  \dropLyrics
  Ri -- u, ri -- u Chi -- u, la guar -- da ri -- be -- ra.
  \dropLyricsIV
  Dios guar -- dó el lo -- bo,~el
  \dropLyrics
  lo -- bo de nues -- tra cor -- de -- ra.
  
  \dropLyricsIV
  Dios guar -- dó el lo -- bo,~el
  \dropLyrics
  lo -- bo de nues -- tra cor -- de -- ra.
  
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
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
  s4*26 |
  
  c8 c c a |
  a4 a8 a4 a8 d, d |
  e4 e s |
  c'8 c b c |
  a4 b8 a |
  
  a4 a8 a4 a8 a a |
  a4 a s |
  c8 c b c |
  a4 b8 a |
  a4 a8 f4 f8 e e |
  d4 d \bar "|."
}
tenorWords = \lyricmode {
  \repeat unfold 12 {\skip1}
  Dios guar -- dó el lo -- bo, el
  \repeat unfold 8 {\skip1}
  Dios guar -- dó el lo -- bo, el
}

bassMusic = \relative c' {
  \oneVoice
  \partial 2 a8 a g a |
  f4 e8 d4 e8 f g |
  \time 3/4 a4 a r |
  \time 2/4 a8 a g a |
  
  \time 4/4 f4 e8 g4 g8 e f |
  \time 3/4 d4 d r |
  \time 2/4 a'8 a g a |
  \time 4/4 f4 e8 g4 g8 e f |
  \partial 2 d4 d \bar "||" \break
  
  \voiceTwo
  \partial 2 f8 f c f |
  d4 a8 d4 c8 bes bes |
  \time 3/4 a4 a d\rest |
  \time 2/4 a'8 a g a |
  f4 e8 f |
  
  \time 4/4 d4 a8 d4 d8 a a |
  \time 3/4 d4 d d\rest |
  \time 2/4 a'8 a g a |
  f4 e8 f |
  \time 4/4 d4 a8 d4 d8 a a |
  \partial 2 d4 d \bar "||" \break
  
  \oneVoice
  \slurSolid
  {
    %Verse 1
    a'8 a g a f4 e8 g( |
    g) g e f d2 |
    a'8 a g a f4 f8 e |
    
    g g e f d2 |
    a'8 a g a f4 f8 e |
    d e f g a2 |
    
    a8 a g a f4 f8 e |
    g g e f d4 d \bar "||" \break
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark "D.S."
  }
  
  
  {
    %Verse 2
    a'8 a g a f4 e8 g( |
    g) g e f d4 d |
    a'8 a g a f4 f8 e |
    
    g4 e8 f d4 d |
    a'8 a g a f4 f8 e |
    d e f g a4 a4 |
    
    a8 a g a f4 f8 e |
    g g e f d4 d \bar "||" \break
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark "D.S."
  }
  {
    %Verse 3
    a'8 a g a f4 e8 g~ |
    g g e f d4 d |
    a'8 a g a f4 f8 e |
    
    g g e f d4 d |
    a'8 a g a f4. e8 |
    d e f g a4 a4 |
    
    a8 a g a f4 f8 e |
    g4 e8 f d4 d \bar "||" \break
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark "D.S."
  }
  
  {
    %Verse 4
    a'8 a g a f4 e8 g( |
    g) g e f d4 d |
    a'8 a g a f4 f8 e |
    
    g4 e8 f d4 d |
    a'8 a g a f4. e8 |
    d e f g a4 a4 |
    
    a8 a g a f4. e8 |
    g g e f d4 d \bar "||" \break
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark "D.S."
  }
  {
    %Verse 5
    a'8 a g a f4 e8 g~ |
    g4 e8 f d4 d8 d |
    a'8 a g a f4. e8 |
    
    g g e f d4 d |
    a'8 a g a f4 f8 e |
    d( e) f g a4 a4 |
    
    a8 a g a f4. e8 |
    g8 g e f d4 d \bar "||" \break
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark "D.S."
  }
  {
    %Verse 6
    a'8 a g a f4 e8 g( |
    g) g e f d4 d |
    a'8 a g a f4 f8 e |
    
    g g e f d4 d |
    a'8 a g a f4. e8 |
    d e f g a2 |
    
    a8 a g a f4. e8 |
    g8 g e f d4 d \bar "||" \break
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark "D.S."
  }
  {
    %Verse 7
    a'8 a g a f4 e8 g( |
    g) g e f d4 d |
    a'8 a g a f4 f8 e |
    
    g4 e8 f d4 d |
    a'8 a g a f4. e8 |
    d e f g a2 |
    
    a8 a g a f4 f8 e |
    g4 e8 f d4 d \bar "||" \break
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark "D.S."
  }
}
bassWords = \lyricmode {
  \dropLyricsIV
  Ri -- u, ri -- u Chi -- u, la guar -- da ri -- be -- ra.
  Dios guar -- dó el lo -- bo de nues -- tra cor -- de -- ra.
  Dios guar -- dó el lo -- bo de nues -- tra cor -- de -- ra.
  
  \repeat unfold 42 {\skip1}
  \set stanza = #"1. "
  El lo -- bo ra -- bio -- so la __ qui -- so mor -- der,
  Mas Dios po -- de -- ro -- so la su -- po de -- fen -- der,
  Quí -- so -- le ha -- zer que no pu -- dies -- se pe -- car:
  Ni~aún o -- ri -- gi -- nal es -- ta Vir -- gen no tu -- vie -- ra.
  
  \set stanza = #"2. "
  Es -- te qu’es nas -- çi -- do es __ el gran mo -- nar -- cha,
  Chris -- to pa -- tri -- ar -- cha de car -- ne ves -- ti -- do.
  Ha -- nos re -- di -- mi -- do con se ha -- zer chi -- qui -- to,
  Aun -- qu’e -- ra~in -- fi -- ni -- to fi -- ni -- to ses hi -- zie -- ra.
  
  \set stanza = #"3. "
  Mu -- chas pro -- fe -- cí -- as lo~han pro -- fe -- ti -- za -- do,
  Y~aún en nue -- stros dí -- as, lo he -- mos al -- can -- ça -- do;
  A Dios hu -- ma -- na -- do ve -- mos en el sue -- lo,
  Y~al hom -- bre~en el cie -- lo por -- que’l le qui -- sie -- ra.
  
  
  \set stanza = #"4. "
  Yo vi mil gar -- ço -- nes que~an -- da -- van can -- tan -- do,
  Por a -- quí vo -- lan -- do ha -- zien -- do mil so -- nes,
  Di -- zien -- do~a ga -- sco -- nes: Glo -- ria sea~en el cie -- lo
  Y paz en el sue -- lo, pues Je -- sús nas -- çie -- ra.
  
  \set stanza = #"5. "
  Es -- te vie -- ne~a dar a los __ muer -- tos vi -- da,
  Y vie -- ne~a re -- pa -- rar de to -- dos la ca -- y -- da.
  Es la luz del dí -- a a -- ques -- te mo -- çue -- lo;
  Es -- t’es el cor -- de -- ro que San Juan di -- xe -- ra.
  
  
  \set stanza = #"6. "
  Mi -- ra bien que~os cua -- dre que~an -- si -- na lo~o -- ye -- ra:
  Que Dios no pu -- die -- ra ha -- zer -- la más que Ma -- dre;
  El qu’e -- ra su Pa -- dre, hoy d’e -- lla nas -- çió
  Y~el que la cri -- ó, su Hi -- jo se di -- xe -- ra.
  
  \set stanza = #"7. "
  Pues que ya te -- ne -- mos lo __ que de -- se -- a -- mos,
  To -- dos jun -- tos va -- mos, pre -- sen -- tes lle -- ve -- mos;
  To -- dos le da -- re -- mos nue -- stra vo -- lun -- tad,
  Pues a se~i -- gua -- lar con el hom -- bre vi -- nie -- ra.
}

bassWordsII = \lyricmode {
  \repeat unfold 78{\skip1}
  
  
}

bassWordsIII = \lyricmode {
  \repeat unfold 166 \skip1
  \set ignoreMelismata = ##t
  
}

bassWordsIV = \lyricmode {
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWordsIV
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWordsIII
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWordsII
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
       \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

