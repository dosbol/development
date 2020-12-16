package discourje.core.validation;

import discourje.core.validation.rules.Causality;
import discourje.core.validation.rules.CloseChannelsOnlyOnce;
import discourje.core.validation.rules.ClosedChannelMustBeUsedInPath;
import discourje.core.validation.rules.ClosedChannelMustBeUsedInProtocol;
import discourje.core.validation.rules.DoNotSendAfterClose;
import discourje.core.validation.rules.DoNotSendToSelf;
import discourje.core.validation.rules.UsedChannelsMustBeClosed;
import java.util.List;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ModelCheckerTest<Spec> extends AbstractModelCheckerTest<Spec> {

    @Test
    public void testCausalityTrivialCorrect() {
        List<String> result = getModelCheckerResult("causality-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testCausalityTrivialIncorrect() {
        List<String> result = getModelCheckerResult("causality-trivial-incorrect");
        assertEquals(1, result.size());
        assertTrue(result.contains(new Causality().createErrorDescription("c", "a")));
    }

    @Test
    public void testCausalityNonTrivialCorrect() {
        List<String> result = getModelCheckerResult("causality-non-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testCausalityNonTrivialIncorrect() {
        List<String> result = getModelCheckerResult("causality-non-trivial-incorrect");
        assertEquals(1, result.size());
        assertTrue(result.contains(new Causality().createErrorDescription("a", "b")));
    }

    @Test
    public void testCloseChannelsOnlyOnceTrivialCorrect() {
        List<String> result = getModelCheckerResult("close-channels-only-once-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testCloseChannelsOnlyOnceTrivialIncorrect() {
        List<String> result = getModelCheckerResult("close-channels-only-once-trivial-incorrect");
        assertEquals(1, result.size());
        assertTrue(result.contains(new CloseChannelsOnlyOnce().createErrorDescription("a", "b")));
    }

    @Test
    public void testCloseChannelsOnlyOnceNonTrivialCorrect() {
        List<String> result = getModelCheckerResult("close-channels-only-once-non-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testCloseChannelsOnlyOnceNonTrivialIncorrect() {
        List<String> result = getModelCheckerResult("close-channels-only-once-non-trivial-incorrect");
        assertEquals(2, result.size());
        assertTrue(result.contains(new CloseChannelsOnlyOnce().createErrorDescription("a", "b")));
        assertTrue(result.contains(new DoNotSendAfterClose().createErrorDescription("a", "b")));
    }

    @Test
    public void testClosedChannelMustBeUsedInProtocolTrivialCorrect() {
        List<String> result = getModelCheckerResult("closed-channel-must-be-used-in-protocol-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testClosedChannelMustBeUsedInProtocolTrivialIncorrect() {
        List<String> result = getModelCheckerResult("closed-channel-must-be-used-in-protocol-trivial-incorrect");
        assertEquals(2, result.size());
        assertTrue(result.contains(new ClosedChannelMustBeUsedInProtocol().createErrorDescription("b", "a")));
        assertTrue(result.contains(new ClosedChannelMustBeUsedInPath().createErrorDescription("b", "a")));
    }

    @Test
    public void testClosedChannelMustBeUsedInProtocolNonTrivialCorrect() {
        List<String> result = getModelCheckerResult("closed-channel-must-be-used-in-protocol-non-trivial-correct");
        // No ClosedChannelMustBeUsedInProtocol errors expected, but we do expect two for ClosedChannelMustBeUsedInPath
        assertEquals(2, result.size());
        assertTrue(result.contains(new ClosedChannelMustBeUsedInPath().createErrorDescription("a", "b")));
        assertTrue(result.contains(new ClosedChannelMustBeUsedInPath().createErrorDescription("b", "a")));
    }

    @Test
    public void testClosedChannelMustBeUsedInProtocolNonTrivialIncorrect() {
        List<String> result = getModelCheckerResult("closed-channel-must-be-used-in-protocol-non-trivial-incorrect");
        assertEquals(3, result.size());
        assertTrue(result.contains(new ClosedChannelMustBeUsedInProtocol().createErrorDescription("b", "c")));
        assertTrue(result.contains(new ClosedChannelMustBeUsedInPath().createErrorDescription("a", "b")));
        assertTrue(result.contains(new ClosedChannelMustBeUsedInPath().createErrorDescription("b", "c")));

    }

    @Test
    public void testClosedChannelMustBeUsedInPathTrivialCorrect() {
        List<String> result = getModelCheckerResult("closed-channel-must-be-used-in-path-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testClosedChannelMustBeUsedInPathTrivialIncorrect() {
        List<String> result = getModelCheckerResult("closed-channel-must-be-used-in-path-trivial-incorrect");
        assertEquals(2, result.size());
        assertTrue(result.contains(new ClosedChannelMustBeUsedInProtocol().createErrorDescription("a", "c")));
        assertTrue(result.contains(new ClosedChannelMustBeUsedInPath().createErrorDescription("a", "c")));
    }

    @Test
    public void testClosedChannelMustBeUsedInPathNonTrivialCorrect() {
        List<String> result = getModelCheckerResult("closed-channel-must-be-used-in-path-non-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testClosedChannelMustBeUsedInPathNonTrivialIncorrect() {
        List<String> result = getModelCheckerResult("closed-channel-must-be-used-in-path-non-trivial-incorrect");
        assertEquals(1, result.size());
        assertTrue(result.contains(new ClosedChannelMustBeUsedInPath().createErrorDescription("a", "c")));
    }

    @Test
    public void testUsedChannelsMustBeClosedTrivialCorrect() {
        List<String> result = getModelCheckerResult("used-channels-must-be-closed-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testUsedChannelsMustBeClosedTrivialIncorrect() {
        List<String> result = getModelCheckerResult("used-channels-must-be-closed-trivial-incorrect");
        assertTrue(result.contains(new UsedChannelsMustBeClosed().createErrorDescription("b", "a")));
    }

    @Test
    public void testUsedChannelsMustBeClosedNonTrivialCorrect() {
        List<String> result = getModelCheckerResult("used-channels-must-be-closed-non-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testUsedChannelsMustBeClosedNonTrivialIncorrect() {
        List<String> result = getModelCheckerResult("used-channels-must-be-closed-non-trivial-incorrect");
        assertTrue(result.contains(new UsedChannelsMustBeClosed().createErrorDescription("d", "a")));
    }

    @Test
    public void testDoNotSendAfterCloseTrivialCorrect() {
        List<String> result = getModelCheckerResult("do-not-send-after-close-trivial-correct");
        assertFalse(result.contains(new DoNotSendAfterClose().createErrorDescription("a", "b")));
    }

    @Test
    public void testDoNotSendAfterCloseTrivialIncorrect() {
        List<String> result = getModelCheckerResult("do-not-send-after-close-trivial-incorrect");
        assertTrue(result.contains(new DoNotSendAfterClose().createErrorDescription("a", "b")));
    }

    @Test
    public void testDoNotSendAfterCloseNonTrivialCorrect() {
        List<String> result = getModelCheckerResult("do-not-send-after-close-non-trivial-correct");
        assertFalse(result.contains(new DoNotSendAfterClose().createErrorDescription("a", "b")));
    }

    @Test
    public void testDoNotSendAfterCloseNonTrivialIncorrect() {
        List<String> result = getModelCheckerResult("do-not-send-after-close-non-trivial-incorrect");
        assertTrue(result.contains(new DoNotSendAfterClose().createErrorDescription("a", "b")));
    }

    @Test
    public void testDoNotSendToSelfTrivialCorrect() {
        List<String> result = getModelCheckerResult("do-not-send-to-self-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testDoNotSendToSelfTrivialIncorrect() {
        List<String> result = getModelCheckerResult("do-not-send-to-self-trivial-incorrect");
        assertTrue(result.contains(new DoNotSendToSelf().createErrorDescription("b", "b")));
    }

    @Test
    public void testDoNotSendToSelfNonTrivialCorrect() {
        List<String> result = getModelCheckerResult("do-not-send-to-self-non-trivial-correct");
        assertTrue(result.isEmpty());
    }

    @Test
    public void testDoNotSendToSelfNonTrivialIncorrect() {
        List<String> result = getModelCheckerResult("do-not-send-to-self-non-trivial-incorrect");
        assertTrue(result.contains(new DoNotSendToSelf().createErrorDescription("a", "b")));
    }
}