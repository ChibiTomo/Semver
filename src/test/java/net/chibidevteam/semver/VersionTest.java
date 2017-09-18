package net.chibidevteam.semver;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

import lombok.extern.slf4j.Slf4j;
import net.chibidevteam.semver.exceptions.MalformedVersionException;

@Slf4j
public class VersionTest {

    @Test
    public void compare() {
        checkGreaterThan("1.2", "1");
        checkGreaterThan("1.2.6", "1.2.6-alpha");

        checkLowerThan("1", "1.2");
        checkLowerThan("1.0.0-alpha", "1.0.0-beta");

        checkSame("1.2", "1.2");
        checkSame("1.2+256", "1.2+2017");
    }

    private void checkGreaterThan(String v1Str, String v2Str) {
        try {
            Version v1 = VersionHelper.deserialize(v1Str);
            Version v2 = VersionHelper.deserialize(v2Str);
            assertTrue("Expect '" + v1Str + "' > '" + v2Str + "', should be '" + v1Str + "'.compareTo('" + v2Str
                    + "') > 0", v1.compareTo(v2) > 0);
        } catch (MalformedVersionException e) {
            log.error("Cannot create version: Check that VersionHelper is working", e);
            fail("Cannot create version: Check that VersionHelper is working: " + e.getMessage());
        }
    }

    private void checkLowerThan(String v1Str, String v2Str) {
        try {
            Version v1 = VersionHelper.deserialize(v1Str);
            Version v2 = VersionHelper.deserialize(v2Str);
            assertTrue("Expect '" + v1Str + "' < '" + v2Str + "', should be '" + v1Str + "'.compareTo('" + v2Str
                    + "') < 0", v1.compareTo(v2) < 0);
        } catch (MalformedVersionException e) {
            log.error("Cannot create version: Check that VersionHelper is working", e);
            fail("Cannot create version: Check that VersionHelper is working");
        }
    }

    private void checkSame(String v1Str, String v2Str) {
        try {
            Version v1 = VersionHelper.deserialize("1.2");
            Version v2 = VersionHelper.deserialize("1.2");
            assertTrue("Expect '" + v1Str + "' ~= '" + v2Str + "', should be '" + v1Str + "'.compareTo('" + v2Str
                    + "') = 0", v1.compareTo(v2) == 0);
        } catch (MalformedVersionException e) {
            log.error("Cannot create version: Check that VersionHelper is working", e);
            fail("Cannot create version: Check that VersionHelper is working");
        }
    }
}
