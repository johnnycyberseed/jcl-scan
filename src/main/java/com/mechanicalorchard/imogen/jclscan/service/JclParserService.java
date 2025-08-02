package com.mechanicalorchard.imogen.jclscan.service;

import com.mechanicalorchard.imogen.jclscan.model.JclFile;
import com.mechanicalorchard.imogen.jclscan.model.JclStep;
import com.mechanicalorchard.imogen.jclscan.model.ProcRef;
import com.mechanicalorchard.imogen.jclscan.model.ProgRef;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.stereotype.Service;

@Service
public class JclParserService {

    private static final Pattern JOB_OR_PROC_PATTERN = Pattern.compile("^//([A-Z0-9]+)\\s+(JOB\\s+.*|PROC\\s*.*)", Pattern.MULTILINE);
    private static final Pattern STEP_PATTERN = Pattern.compile("^//([A-Z0-9]+)\\s+EXEC\\s+(PGM=([A-Z0-9]+)|PROC=([A-Z0-9]+)).*", Pattern.MULTILINE);

    public JclFile parse(String jclContent) {
        String fileName = extractJobOrProcName(jclContent);
        List<JclStep> steps = extractSteps(jclContent);
        
        return JclFile.builder()
            .name(fileName)
            .steps(steps)
            .build();
    }

    private String extractJobOrProcName(String jclContent) {
        Matcher matcher = JOB_OR_PROC_PATTERN.matcher(jclContent);
        if (matcher.find()) {
            return matcher.group(1);
        }
        throw new IllegalArgumentException("No JOB or PROC statement found in JCL content");
    }

    private List<JclStep> extractSteps(String jclContent) {
        List<JclStep> steps = new ArrayList<>();
        Matcher matcher = STEP_PATTERN.matcher(jclContent);
        
        while (matcher.find()) {
            String stepName = matcher.group(1);
            String pgmName = matcher.group(3);  // PGM value
            String procName = matcher.group(4); // PROC value
            
            // Create ProgRef if we have a program name, otherwise null
            ProgRef progRef = pgmName != null ? 
                ProgRef.builder().name(pgmName).build() : null;
            
            // Create ProcRef if we have a procedure name, otherwise null
            ProcRef procRef = procName != null ? 
                ProcRef.builder().name(procName).build() : null;
            
            steps.add(JclStep.builder()
                .name(stepName)
                .pgm(progRef)
                .proc(procRef)
                .build());
        }
        
        return steps;
    }
}