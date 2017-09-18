package net.chibidevteam.semver;

import static net.chibidevteam.semver.Constants.INTEGER_COMPARATOR;
import static net.chibidevteam.semver.Constants.STRING_COMPARATOR;

import java.util.Arrays;
import java.util.List;

import lombok.Value;

@Value
public class Version implements Comparable<Version> {

    private Integer      major;
    private Integer      minor;
    private Integer      patch;
    private String       preRelease;
    private List<String> meta;

    public Version(Integer major, Integer minor, Integer patch, String preRelease, String... meta) {
        this(major, minor, patch, preRelease, Arrays.asList(meta));
    }

    public Version(Integer major, Integer minor, Integer patch, String preRelease, List<String> meta) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
        this.preRelease = preRelease;
        this.meta = meta;
    }

    @Override
    public int compareTo(Version v) {
        int result = INTEGER_COMPARATOR.compare(major, v.major);
        if (result != 0) {
            return result;
        }
        result = INTEGER_COMPARATOR.compare(minor, v.minor);
        if (result != 0) {
            return result;
        }
        result = INTEGER_COMPARATOR.compare(patch, v.patch);
        if (result != 0) {
            return result;
        }
        return STRING_COMPARATOR.compare(preRelease, v.preRelease);
    }
}
