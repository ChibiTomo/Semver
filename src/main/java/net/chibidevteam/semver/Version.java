package net.chibidevteam.semver;

import static net.chibidevteam.semver.Constants.INTEGER_COMPARATOR;
import static net.chibidevteam.semver.Constants.STRING_COMPARATOR;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import lombok.Value;

@Value
public class Version implements Comparable<Version> {

    private Integer      major;
    private Integer      minor;
    private Integer      patch;
    private String       preRelease;
    private List<String> meta;

    public Version(Integer major, Integer minor, Integer patch, String preRelease, String... meta) {
        this(major, minor, patch, preRelease, meta != null && meta.length > 0 ? Arrays.asList(meta) : null);
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
        if (preRelease == null || v.preRelease == null) {
            return STRING_COMPARATOR.compare(preRelease, v.preRelease);
        }
        return STRING_COMPARATOR.compare(StringUtils.stripAccents(preRelease).toLowerCase(),
                StringUtils.stripAccents(v.preRelease).toLowerCase());
    }
}
