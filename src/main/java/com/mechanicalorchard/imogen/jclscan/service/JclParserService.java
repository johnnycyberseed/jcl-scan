package com.mechanicalorchard.imogen.jclscan.service;

import com.mechanicalorchard.imogen.jclscan.model.JclJob;
import com.mechanicalorchard.imogen.jclscan.model.JclStep;

import java.util.List;

import org.springframework.stereotype.Service;

@Service
public class JclParserService {

    public JclJob parse(String jclContent) {
        return JclJob.builder()
            .name("SIMPLE")
            .steps(List.of(JclStep.builder()
                .name("STEP1")
                .pgm("IEFBR14")
                .build()))
            .build();
    }
}