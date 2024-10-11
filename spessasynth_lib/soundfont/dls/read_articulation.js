import { readLittleEndian } from "../../utils/byte_functions/little_endian.js";
import { DLSDestinations } from "./dls_destinations.js";
import { DLSSources } from "./dls_sources.js";
import { getSF2ModulatorFromArticulator } from "./articulator_converter.js";
import { SpessaSynthInfo, SpessaSynthWarn } from "../../utils/loggin.js";
import { consoleColors } from "../../utils/other.js";
import { Generator, generatorTypes } from "../basic_soundfont/generator.js";
import { Modulator } from "../basic_soundfont/modulator.js";

/**
 * @param source {number}
 * @param control {number}
 * @param destination {number}
 * @param value {number}
 * @param transform {number}
 */
function modulatorConverterDebug(
    source,
    control,
    destination,
    value,
    transform
)
{
    const type = Object.keys(DLSDestinations).find(k => DLSDestinations[k] === destination);
    const srcType = Object.keys(DLSSources).find(k => DLSSources[k] === source);
    const ctrlType = Object.keys(DLSSources).find(k => DLSSources[k] === control);
    const typeString = type ? type : destination.toString(16);
    const srcString = srcType ? srcType : source.toString(16);
    const ctrlString = ctrlType ? ctrlType : control.toString(16);
    SpessaSynthInfo(
        `%cAttempting to convert the following DLS Articulator to SF2 Modulator:
        Source: %c${srcString}%c
        Control: %c${ctrlString}%c
        Destination: %c${typeString}%c
        Amount: %c${value}%c
        Transform: %c${transform}%c...`,
        consoleColors.info,
        consoleColors.recognized,
        consoleColors.info,
        consoleColors.recognized,
        consoleColors.info,
        consoleColors.recognized,
        consoleColors.info,
        consoleColors.recognized,
        consoleColors.info,
        consoleColors.recognized,
        consoleColors.info
    );
}

/**
 * Reads the articulator chunk
 * @param chunk {RiffChunk}
 * @param disableVibrato {boolean} it seems that dls 1 does not have vibrato lfo, so we shall disable it
 * @returns {{modulators: Modulator[], generators: Generator[]}}
 */
export function readArticulation(chunk, disableVibrato)
{
    const artData = chunk.chunkData;
    /**
     * @type {Generator[]}
     */
    const generators = [];
    /**
     * @type {Modulator[]}
     */
    const modulators = [];
    
    // cbSize (ignore)
    readLittleEndian(artData, 4);
    const connectionsAmount = readLittleEndian(artData, 4);
    for (let i = 0; i < connectionsAmount; i++)
    {
        // read the block
        const source = readLittleEndian(artData, 2);
        const control = readLittleEndian(artData, 2);
        const destination = readLittleEndian(artData, 2);
        const transform = readLittleEndian(artData, 2);
        const scale = readLittleEndian(artData, 4) | 0;
        const value = scale >> 16; // convert it to 16 bit as soundfont uses that
        
        // modulatorConverterDebug(source, control, destination, value, transform);
        // interpret this somehow...
        // if source and control are both zero, it's a generator
        if (source === 0 && control === 0 && transform === 0)
        {
            /**
             * @type {Generator}
             */
            let generator;
            switch (destination)
            {
                case DLSDestinations.pan:
                    generator = new Generator(generatorTypes.pan, value); // turn percent into tenths of percent
                    break;
                case DLSDestinations.gain:
                    generator = new Generator(generatorTypes.initialAttenuation, -value * 10 / 0.4); // turn to centibels and apply emu correction
                    break;
                case DLSDestinations.filterCutoff:
                    generator = new Generator(generatorTypes.initialFilterFc, value);
                    break;
                case DLSDestinations.filterQ:
                    generator = new Generator(generatorTypes.initialFilterQ, value);
                    break;
                
                // mod lfo raw values it seems
                case DLSDestinations.modLfoFreq:
                    generator = new Generator(generatorTypes.freqModLFO, value);
                    break;
                case DLSDestinations.modLfoDelay:
                    generator = new Generator(generatorTypes.delayModLFO, value);
                    break;
                case DLSDestinations.vibLfoFreq:
                    generator = new Generator(generatorTypes.freqVibLFO, value);
                    break;
                case DLSDestinations.vibLfoDelay:
                    generator = new Generator(generatorTypes.delayVibLFO, value);
                    break;
                
                // vol env: all times are timecents like sf2
                case DLSDestinations.volEnvDelay:
                    generator = new Generator(generatorTypes.delayVolEnv, value);
                    break;
                case DLSDestinations.volEnvAttack:
                    generator = new Generator(generatorTypes.attackVolEnv, value);
                    break;
                case DLSDestinations.volEnvHold:
                    generator = new Generator(generatorTypes.holdVolEnv, value);
                    break;
                case DLSDestinations.volEnvDecay:
                    generator = new Generator(generatorTypes.decayVolEnv, value);
                    break;
                case DLSDestinations.volEnvRelease:
                    generator = new Generator(generatorTypes.releaseVolEnv, value);
                    break;
                case DLSDestinations.volEnvSustain:
                    // gain seems to be (1000 - value) / 10 = sustain dB
                    const sustainDb = (1000 - value) / 10;
                    generator = new Generator(generatorTypes.sustainVolEnv, sustainDb * 10);
                    break;
                
                // mod env
                case DLSDestinations.modEnvDelay:
                    generator = new Generator(generatorTypes.delayModEnv, value);
                    break;
                case DLSDestinations.modEnvAttack:
                    generator = new Generator(generatorTypes.attackModEnv, value);
                    break;
                case DLSDestinations.modEnvHold:
                    generator = new Generator(generatorTypes.holdModEnv, value);
                    break;
                case DLSDestinations.modEnvDecay:
                    generator = new Generator(generatorTypes.decayModEnv, value);
                    break;
                case DLSDestinations.modEnvRelease:
                    generator = new Generator(generatorTypes.releaseModEnv, value);
                    break;
                case DLSDestinations.modEnvSustain:
                    // dls uses 1%, desfont uses 0.1%
                    const percentageSustain = 1000 - value;
                    generator = new Generator(generatorTypes.sustainModEnv, percentageSustain);
                    break;
                
                case DLSDestinations.reverbSend:
                    generator = new Generator(generatorTypes.reverbEffectsSend, value);
                    break;
                case DLSDestinations.chorusSend:
                    generator = new Generator(generatorTypes.chorusEffectsSend, value);
                    break;
                case DLSDestinations.pitch:
                    // split it up
                    const semi = Math.floor(value / 100);
                    const cents = Math.floor(value - semi * 100);
                    generator = new Generator(generatorTypes.fineTune, cents);
                    generators.push(new Generator(generatorTypes.coarseTune, semi));
                    break;
            }
            if (generator)
            {
                generators.push(generator);
            }
        }
        else
            // if not, modulator??
        {
            let isGenerator = true;
            // a few special cases which are generators:
            if (control === DLSSources.none)
            {
                // mod lfo to pitch
                if (source === DLSSources.modLfo && destination === DLSDestinations.pitch)
                {
                    generators.push(new Generator(generatorTypes.modLfoToPitch, value));
                }
                else
                    // mod lfo to volume
                if (source === DLSSources.modLfo && destination === DLSDestinations.gain)
                {
                    generators.push(new Generator(generatorTypes.modLfoToVolume, value));
                }
                else
                    // mod lfo to filter
                if (source === DLSSources.modLfo && destination === DLSDestinations.filterCutoff)
                {
                    generators.push(new Generator(generatorTypes.modLfoToFilterFc, value));
                }
                else
                    // vib lfo to pitch
                if (source === DLSSources.vibratoLfo && destination === DLSDestinations.pitch)
                {
                    generators.push(new Generator(generatorTypes.vibLfoToPitch, value));
                }
                else
                    // mod env to pitch
                if (source === DLSSources.modEnv && destination === DLSDestinations.pitch)
                {
                    generators.push(new Generator(generatorTypes.modEnvToPitch, value));
                }
                else
                    // mod env to filter
                if (source === DLSSources.modEnv && destination === DLSDestinations.filterCutoff)
                {
                    generators.push(new Generator(generatorTypes.modEnvToFilterFc, value));
                }
                else
                    // key to vol env hold
                if (source === DLSSources.keyNum && destination === DLSDestinations.volEnvHold)
                {
                    // according to viena and another strange (with modulators) rendition of gm.dls in sf2,
                    // it shall be divided by -128
                    // and a strange correction needs to be applied to the real value:
                    // real + (60 / 128) * scale
                    generators.push(new Generator(generatorTypes.keyNumToVolEnvHold, value / -128));
                    const correction = Math.round((60 / 128) * value);
                    generators.forEach(g =>
                    {
                        if (g.generatorType === generatorTypes.holdVolEnv)
                        {
                            g.generatorValue += correction;
                        }
                    });
                }
                else
                    // key to vol env decay
                if (source === DLSSources.keyNum && destination === DLSDestinations.volEnvDecay)
                {
                    // according to viena and another strange (with modulators) rendition of gm.dls in sf2,
                    // it shall be divided by -128
                    // and a strange correction needs to be applied to the real value:
                    // real + (60 / 128) * scale
                    generators.push(new Generator(generatorTypes.keyNumToVolEnvDecay, value / -128));
                    const correction = Math.round((60 / 128) * value);
                    generators.forEach(g =>
                    {
                        if (g.generatorType === generatorTypes.decayVolEnv)
                        {
                            g.generatorValue += correction;
                        }
                    });
                }
                else
                    // key to mod env hold
                if (source === DLSSources.keyNum && destination === DLSDestinations.modEnvHold)
                {
                    // according to viena and another strange (with modulators) rendition of gm.dls in sf2,
                    // it shall be divided by -128
                    // and a strange correction needs to be applied to the real value:
                    // real + (60 / 128) * scale
                    generators.push(new Generator(generatorTypes.keyNumToModEnvHold, value / -128));
                    const correction = Math.round((60 / 128) * value);
                    generators.forEach(g =>
                    {
                        if (g.generatorType === generatorTypes.holdModEnv)
                        {
                            g.generatorValue += correction;
                        }
                    });
                }
                else
                    // key to mod env decay
                if (source === DLSSources.keyNum && destination === DLSDestinations.modEnvDecay)
                {
                    // according to viena and another strange (with modulators) rendition of gm.dls in sf2,
                    // it shall be divided by -128
                    // and a strange correction needs to be applied to the real value:
                    // real + (60 / 128) * scale
                    generators.push(new Generator(generatorTypes.keyNumToModEnvDecay, value / -128));
                    const correction = Math.round((60 / 128) * value);
                    generators.forEach(g =>
                    {
                        if (g.generatorType === generatorTypes.decayModEnv)
                        {
                            g.generatorValue += correction;
                        }
                    });
                }
                else
                {
                    isGenerator = false;
                }
                
            }
            else
            {
                isGenerator = false;
            }
            if (isGenerator === false)
            {
                // UNCOMMENT TO ENABLE DEBUG
                // modulatorConverterDebug(source, control, destination, value, transform)
                // convert it to modulator
                const mod = getSF2ModulatorFromArticulator(
                    source,
                    control,
                    destination,
                    transform,
                    value
                );
                if (mod)
                {
                    // some articulators cannot be turned into modulators, that's why this check is a thing
                    modulators.push(mod);
                    SpessaSynthInfo("%cSucceeded converting to SF2 Modulator!", consoleColors.recognized);
                }
                else
                {
                    SpessaSynthWarn("Failed converting to SF2 Modulator!");
                }
            }
        }
    }
    
    // override reverb and chorus with 1000 instead of 200 (if not overriden)
    // reverb
    if (modulators.find(m => m.modulatorDestination === generatorTypes.reverbEffectsSend) === undefined)
    {
        modulators.push(new Modulator({
            srcEnum: 0x00DB,
            dest: generatorTypes.reverbEffectsSend,
            amt: 1000,
            secSrcEnum: 0x0,
            transform: 0
        }));
    }
    // chorus
    if (modulators.find(m => m.modulatorDestination === generatorTypes.chorusEffectsSend) === undefined)
    {
        modulators.push(new Modulator({
            srcEnum: 0x00DD,
            dest: generatorTypes.chorusEffectsSend,
            amt: 1000,
            secSrcEnum: 0x0,
            transform: 0
        }));
    }
    
    // it seems that dls 1 does not have vibrato lfo, so we shall disable it
    if (disableVibrato)
    {
        modulators.push(
            // mod to vib
            new Modulator({
                srcEnum: 0x0081,
                dest: generatorTypes.vibLfoToPitch,
                amt: 0,
                secSrcEnum: 0x0,
                transform: 0
            }),
            // press to vib
            new Modulator({
                srcEnum: 0x000D,
                dest: generatorTypes.vibLfoToPitch,
                amt: 0,
                secSrcEnum: 0x0,
                transform: 0
            })
        );
    }
    
    return { modulators: modulators, generators: generators };
}