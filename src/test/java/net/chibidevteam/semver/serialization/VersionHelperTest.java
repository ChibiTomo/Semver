package net.chibidevteam.semver.serialization;

import static net.chibidevteam.semver.Constants.DELIMITER_META;
import static net.chibidevteam.semver.Constants.DELIMITER_PRE_RELEASE;
import static net.chibidevteam.semver.Constants.SEPARATOR_META;
import static net.chibidevteam.semver.Constants.SEPARATOR_VERSION;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.Arrays;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;

import lombok.extern.slf4j.Slf4j;
import net.chibidevteam.semver.Version;
import net.chibidevteam.semver.SemverHelper;
import net.chibidevteam.semver.exceptions.MalformedVersionException;

@Slf4j
public class VersionHelperTest {

    @Test
    public void serialize() {
        assertEquals("1", SemverHelper.serializeVersion(1));
        assertEquals("1.2", SemverHelper.serializeVersion(1, 2));
        assertEquals("1.2.5", SemverHelper.serializeVersion(1, 2, 5));
        assertEquals("1.2.5-RC2", SemverHelper.serializeVersion(1, 2, 5, "RC2"));
        assertEquals("1.2.5-RC2+sha256", SemverHelper.serializeVersion(1, 2, 5, "RC2", "sha256"));
        assertEquals("1.2.5-RC2+sha256.2017", SemverHelper.serializeVersion(1, 2, 5, "RC2", "sha256", "2017"));

        assertEquals(".2.5-RC2+sha256.2017", SemverHelper.serializeVersion(null, 2, 5, "RC2", "sha256", "2017"));
        assertEquals("1.2.5-RC2+.2017", SemverHelper.serializeVersion(1, 2, 5, "RC2", null, "2017"));
        assertEquals("-RC2+.2017", SemverHelper.serializeVersion(null, null, null, "RC2", null, "2017"));
    }

    @Test
    public void deserialize() {
        deserialize(1, 2, 5, "RC2", "sha256", "2017");
        deserialize(1, 2, null, "RC2", "sha256", "2017");
        deserialize(1);
        deserialize(1, 5);
        deserialize(1, 5, 9);
        deserializeFail(null, 2, 5, "alpha");
    }

    private void deserializeFail(Integer major, Integer minor, Integer patch, String preRelease, String... meta) {
        String tested = getString(major, minor, patch, preRelease, meta);

        log.info("Testing failing version: {}", tested);
        try {
            SemverHelper.deserializeVersion(tested);
        } catch (MalformedVersionException e) {
            log.info("[SUCCESS] Cannot build '" + tested + "': " + e.getMessage());
        }
    }

    private void deserialize(Integer major) {
        deserialize(major, null, null, null);
    }

    private void deserialize(Integer major, Integer minor) {
        deserialize(major, minor, null, null);
    }

    private void deserialize(Integer major, Integer minor, Integer patch) {
        deserialize(major, minor, patch, null);
    }

    private void deserialize(Integer major, Integer minor, Integer patch, String preRelease, String... meta) {
        Version expected = new Version(major, minor, patch, preRelease,
                meta != null && meta.length > 0 ? Arrays.asList(meta) : null);
        String tested = getString(major, minor, patch, preRelease, meta);

        log.info("Testing version: {}", tested);
        try {
            assertEquals(expected, SemverHelper.deserializeVersion(tested));
        } catch (MalformedVersionException e) {
            fail("Cannot build '" + tested + "': " + e.getMessage());
        }
    }

    private String getString(Integer major, Integer minor, Integer patch, String preRelease, String... meta) {
        StringBuilder sb = new StringBuilder();
        if (major != null) {
            sb.append(major);
        }
        if (minor != null) {
            sb.append(SEPARATOR_VERSION);
            sb.append(minor);
        }
        if (patch != null) {
            sb.append(SEPARATOR_VERSION);
            sb.append(patch);
        }
        if (preRelease != null) {
            sb.append(DELIMITER_PRE_RELEASE);
            sb.append(preRelease);
        }
        if (!ArrayUtils.isEmpty(meta)) {
            sb.append(DELIMITER_META);
            sb.append(StringUtils.join(meta, SEPARATOR_META));
        }

        return sb.toString();
    }

}
